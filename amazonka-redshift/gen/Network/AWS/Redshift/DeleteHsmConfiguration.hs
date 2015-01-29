{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DeleteHsmConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified Amazon Redshift HSM configuration.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteHsmConfiguration.html>
module Network.AWS.Redshift.DeleteHsmConfiguration
    (
    -- * Request
      DeleteHsmConfiguration
    -- ** Request constructor
    , deleteHsmConfiguration
    -- ** Request lenses
    , dhcHsmConfigurationIdentifier

    -- * Response
    , DeleteHsmConfigurationResponse
    -- ** Response constructor
    , deleteHsmConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

newtype DeleteHsmConfiguration = DeleteHsmConfiguration
    { _dhcHsmConfigurationIdentifier :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteHsmConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhcHsmConfigurationIdentifier' @::@ 'Text'
--
deleteHsmConfiguration :: Text -- ^ 'dhcHsmConfigurationIdentifier'
                       -> DeleteHsmConfiguration
deleteHsmConfiguration p1 = DeleteHsmConfiguration
    { _dhcHsmConfigurationIdentifier = p1
    }

-- | The identifier of the Amazon Redshift HSM configuration to be deleted.
dhcHsmConfigurationIdentifier :: Lens' DeleteHsmConfiguration Text
dhcHsmConfigurationIdentifier =
    lens _dhcHsmConfigurationIdentifier
        (\s a -> s { _dhcHsmConfigurationIdentifier = a })

data DeleteHsmConfigurationResponse = DeleteHsmConfigurationResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteHsmConfigurationResponse' constructor.
deleteHsmConfigurationResponse :: DeleteHsmConfigurationResponse
deleteHsmConfigurationResponse = DeleteHsmConfigurationResponse

instance ToPath DeleteHsmConfiguration where
    toPath = const "/"

instance ToQuery DeleteHsmConfiguration where
    toQuery DeleteHsmConfiguration{..} = mconcat
        [ "HsmConfigurationIdentifier" =? _dhcHsmConfigurationIdentifier
        ]

instance ToHeaders DeleteHsmConfiguration

instance AWSRequest DeleteHsmConfiguration where
    type Sv DeleteHsmConfiguration = Redshift
    type Rs DeleteHsmConfiguration = DeleteHsmConfigurationResponse

    request  = post "DeleteHsmConfiguration"
    response = nullResponse DeleteHsmConfigurationResponse
