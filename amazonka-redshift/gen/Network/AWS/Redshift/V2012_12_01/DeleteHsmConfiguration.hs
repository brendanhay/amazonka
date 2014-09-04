{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DeleteHsmConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified Amazon Redshift HSM configuration.
module Network.AWS.Redshift.V2012_12_01.DeleteHsmConfiguration
    (
    -- * Request
      DeleteHsmConfiguration
    -- ** Request constructor
    , mkDeleteHsmConfigurationMessage
    -- ** Request lenses
    , dhcmHsmConfigurationIdentifier

    -- * Response
    , DeleteHsmConfigurationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteHsmConfiguration' request.
mkDeleteHsmConfigurationMessage :: Text -- ^ 'dhcmHsmConfigurationIdentifier'
                                -> DeleteHsmConfiguration
mkDeleteHsmConfigurationMessage p1 = DeleteHsmConfiguration
    { _dhcmHsmConfigurationIdentifier = p1
    }
{-# INLINE mkDeleteHsmConfigurationMessage #-}

newtype DeleteHsmConfiguration = DeleteHsmConfiguration
    { _dhcmHsmConfigurationIdentifier :: Text
      -- ^ The identifier of the Amazon Redshift HSM configuration to be
      -- deleted.
    } deriving (Show, Generic)

-- | The identifier of the Amazon Redshift HSM configuration to be deleted.
dhcmHsmConfigurationIdentifier :: Lens' DeleteHsmConfiguration (Text)
dhcmHsmConfigurationIdentifier = lens _dhcmHsmConfigurationIdentifier (\s a -> s { _dhcmHsmConfigurationIdentifier = a })
{-# INLINE dhcmHsmConfigurationIdentifier #-}

instance ToQuery DeleteHsmConfiguration where
    toQuery = genericQuery def

data DeleteHsmConfigurationResponse = DeleteHsmConfigurationResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteHsmConfiguration where
    type Sv DeleteHsmConfiguration = Redshift
    type Rs DeleteHsmConfiguration = DeleteHsmConfigurationResponse

    request = post "DeleteHsmConfiguration"
    response _ = nullaryResponse DeleteHsmConfigurationResponse
