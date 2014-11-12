{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Redshift.DeleteHsmConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified Amazon Redshift HSM configuration.
module Network.AWS.Redshift.DeleteHsmConfiguration
    (
    -- * Request
      DeleteHsmConfigurationMessage
    -- ** Request constructor
    , deleteHsmConfigurationMessage
    -- ** Request lenses
    , dhcm1HsmConfigurationIdentifier

    -- * Response
    , DeleteHsmConfigurationResponse
    -- ** Response constructor
    , deleteHsmConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

newtype DeleteHsmConfigurationMessage = DeleteHsmConfigurationMessage
    { _dhcm1HsmConfigurationIdentifier :: Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteHsmConfigurationMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhcm1HsmConfigurationIdentifier' @::@ 'Text'
--
deleteHsmConfigurationMessage :: Text -- ^ 'dhcm1HsmConfigurationIdentifier'
                              -> DeleteHsmConfigurationMessage
deleteHsmConfigurationMessage p1 = DeleteHsmConfigurationMessage
    { _dhcm1HsmConfigurationIdentifier = p1
    }

-- | The identifier of the Amazon Redshift HSM configuration to be deleted.
dhcm1HsmConfigurationIdentifier :: Lens' DeleteHsmConfigurationMessage Text
dhcm1HsmConfigurationIdentifier =
    lens _dhcm1HsmConfigurationIdentifier
        (\s a -> s { _dhcm1HsmConfigurationIdentifier = a })
instance ToQuery DeleteHsmConfigurationMessage

instance ToPath DeleteHsmConfigurationMessage where
    toPath = const "/"

data DeleteHsmConfigurationResponse = DeleteHsmConfigurationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteHsmConfigurationResponse' constructor.
deleteHsmConfigurationResponse :: DeleteHsmConfigurationResponse
deleteHsmConfigurationResponse = DeleteHsmConfigurationResponse

instance FromXML DeleteHsmConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteHsmConfigurationResponse"

instance AWSRequest DeleteHsmConfigurationMessage where
    type Sv DeleteHsmConfigurationMessage = Redshift
    type Rs DeleteHsmConfigurationMessage = DeleteHsmConfigurationResponse

    request  = post "DeleteHsmConfiguration"
    response = nullaryResponse DeleteHsmConfigurationResponse
