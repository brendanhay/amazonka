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
    , mkDeleteEventSubscriptionMessage
    -- ** Request lenses
    , desmSubscriptionName

    -- * Response
    , DeleteEventSubscriptionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteEventSubscription' request.
mkDeleteEventSubscriptionMessage :: Text -- ^ 'desmSubscriptionName'
                                 -> DeleteEventSubscription
mkDeleteEventSubscriptionMessage p1 = DeleteEventSubscription
    { _desmSubscriptionName = p1
    }
{-# INLINE mkDeleteEventSubscriptionMessage #-}

newtype DeleteEventSubscription = DeleteEventSubscription
    { _desmSubscriptionName :: Text
      -- ^ The name of the Amazon Redshift event notification subscription
      -- to be deleted.
    } deriving (Show, Generic)

-- | The name of the Amazon Redshift event notification subscription to be
-- deleted.
desmSubscriptionName :: Lens' DeleteEventSubscription (Text)
desmSubscriptionName = lens _desmSubscriptionName (\s a -> s { _desmSubscriptionName = a })
{-# INLINE desmSubscriptionName #-}

instance ToQuery DeleteEventSubscription where
    toQuery = genericQuery def

data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteEventSubscription where
    type Sv DeleteEventSubscription = Redshift
    type Rs DeleteEventSubscription = DeleteEventSubscriptionResponse

    request = post "DeleteEventSubscription"
    response _ = nullaryResponse DeleteEventSubscriptionResponse
