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

-- Module      : Network.AWS.Route53.DeleteReusableDelegationSet
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action deletes a reusable delegation set. To delete a reusable
-- delegation set, send a DELETE request to the
-- 2013-04-01/delegationset/delegation set ID resource. You can delete a
-- reusable delegation set only if there are no associated hosted zones. If
-- your reusable delegation set contains associated hosted zones, you must
-- delete them before you can delete your reusable delegation set. If you try
-- to delete a reusable delegation set that contains associated hosted zones,
-- Route 53 will deny your request with a DelegationSetInUse error.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteReusableDelegationSet.html>
module Network.AWS.Route53.DeleteReusableDelegationSet
    (
    -- * Request
      DeleteReusableDelegationSet
    -- ** Request constructor
    , deleteReusableDelegationSet
    -- ** Request lenses
    , drdsId

    -- * Response
    , DeleteReusableDelegationSetResponse
    -- ** Response constructor
    , deleteReusableDelegationSetResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

newtype DeleteReusableDelegationSet = DeleteReusableDelegationSet
    { _drdsId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteReusableDelegationSet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdsId' @::@ 'Text'
--
deleteReusableDelegationSet :: Text -- ^ 'drdsId'
                            -> DeleteReusableDelegationSet
deleteReusableDelegationSet p1 = DeleteReusableDelegationSet
    { _drdsId = p1
    }

-- | The ID of the reusable delegation set you want to delete.
drdsId :: Lens' DeleteReusableDelegationSet Text
drdsId = lens _drdsId (\s a -> s { _drdsId = a })

data DeleteReusableDelegationSetResponse = DeleteReusableDelegationSetResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteReusableDelegationSetResponse' constructor.
deleteReusableDelegationSetResponse :: DeleteReusableDelegationSetResponse
deleteReusableDelegationSetResponse = DeleteReusableDelegationSetResponse

instance ToPath DeleteReusableDelegationSet where
    toPath DeleteReusableDelegationSet{..} = mconcat
        [ "/2013-04-01/delegationset/"
        , toText _drdsId
        ]

instance ToQuery DeleteReusableDelegationSet where
    toQuery = const mempty

instance ToHeaders DeleteReusableDelegationSet

instance ToXMLRoot DeleteReusableDelegationSet where
    toXMLRoot = const (namespaced ns "DeleteReusableDelegationSet" [])

instance ToXML DeleteReusableDelegationSet

instance AWSRequest DeleteReusableDelegationSet where
    type Sv DeleteReusableDelegationSet = Route53
    type Rs DeleteReusableDelegationSet = DeleteReusableDelegationSetResponse

    request  = delete
    response = nullResponse DeleteReusableDelegationSetResponse
