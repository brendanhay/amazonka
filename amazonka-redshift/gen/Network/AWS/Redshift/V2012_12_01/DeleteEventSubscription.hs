{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DeleteEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an Amazon Redshift event notification subscription.
module Network.AWS.Redshift.V2012_12_01.DeleteEventSubscription
    (
    -- * Request
      DeleteEventSubscription
    -- ** Request constructor
    , mkDeleteEventSubscription
    -- ** Request lenses
    , desSubscriptionName

    -- * Response
    , DeleteEventSubscriptionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
newtype DeleteEventSubscription = DeleteEventSubscription
    { _desSubscriptionName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteEventSubscription' request.
mkDeleteEventSubscription :: Text -- ^ 'desSubscriptionName'
                          -> DeleteEventSubscription
mkDeleteEventSubscription p1 = DeleteEventSubscription
    { _desSubscriptionName = p1
    }
{-# INLINE mkDeleteEventSubscription #-}

-- | The name of the Amazon Redshift event notification subscription to be
-- deleted.
desSubscriptionName :: Lens' DeleteEventSubscription Text
desSubscriptionName =
    lens _desSubscriptionName (\s a -> s { _desSubscriptionName = a })
{-# INLINE desSubscriptionName #-}

instance ToQuery DeleteEventSubscription where
    toQuery = genericQuery def

data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteEventSubscription where
    type Sv DeleteEventSubscription = Redshift
    type Rs DeleteEventSubscription = DeleteEventSubscriptionResponse

    request = post "DeleteEventSubscription"
    response _ = nullaryResponse DeleteEventSubscriptionResponse
