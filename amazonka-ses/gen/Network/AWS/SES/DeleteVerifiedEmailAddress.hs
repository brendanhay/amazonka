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

-- Module      : Network.AWS.SES.DeleteVerifiedEmailAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified email address from the list of verified addresses.
-- The DeleteVerifiedEmailAddress action is deprecated as of the May 15, 2012
-- release of Domain Verification. The DeleteIdentity action is now preferred.
-- This action is throttled at one request per second.
module Network.AWS.SES.DeleteVerifiedEmailAddress
    (
    -- * Request
      DeleteVerifiedEmailAddress
    -- ** Request constructor
    , deleteVerifiedEmailAddress
    -- ** Request lenses
    , dveaEmailAddress

    -- * Response
    , DeleteVerifiedEmailAddressResponse
    -- ** Response constructor
    , deleteVerifiedEmailAddressResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types

newtype DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddress
    { _dveaEmailAddress :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteVerifiedEmailAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dveaEmailAddress' @::@ 'Text'
--
deleteVerifiedEmailAddress :: Text -- ^ 'dveaEmailAddress'
                           -> DeleteVerifiedEmailAddress
deleteVerifiedEmailAddress p1 = DeleteVerifiedEmailAddress
    { _dveaEmailAddress = p1
    }

-- | An email address to be removed from the list of verified addresses.
dveaEmailAddress :: Lens' DeleteVerifiedEmailAddress Text
dveaEmailAddress = lens _dveaEmailAddress (\s a -> s { _dveaEmailAddress = a })

instance ToPath DeleteVerifiedEmailAddress where
    toPath = const "/"

instance ToQuery DeleteVerifiedEmailAddress

data DeleteVerifiedEmailAddressResponse = DeleteVerifiedEmailAddressResponse

-- | 'DeleteVerifiedEmailAddressResponse' constructor.
deleteVerifiedEmailAddressResponse :: DeleteVerifiedEmailAddressResponse
deleteVerifiedEmailAddressResponse = DeleteVerifiedEmailAddressResponse

instance AWSRequest DeleteVerifiedEmailAddress where
    type Sv DeleteVerifiedEmailAddress = SES
    type Rs DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddressResponse

    request  = post "DeleteVerifiedEmailAddress"
    response = const (nullaryResponse DeleteVerifiedEmailAddressResponse)
