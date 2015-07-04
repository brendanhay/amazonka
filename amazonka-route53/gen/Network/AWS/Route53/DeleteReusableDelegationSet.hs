{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.Route53.DeleteReusableDelegationSet
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This action deletes a reusable delegation set. To delete a reusable
-- delegation set, send a @DELETE@ request to the
-- @2013-04-01\/delegationset\/delegation set ID@ resource.
--
-- You can delete a reusable delegation set only if there are no associated
-- hosted zones. If your reusable delegation set contains associated hosted
-- zones, you must delete them before you can delete your reusable
-- delegation set. If you try to delete a reusable delegation set that
-- contains associated hosted zones, Route 53 will deny your request with a
-- @DelegationSetInUse@ error.
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
    -- ** Response lenses
    , drdsrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type containing the information for the delete request.
--
-- /See:/ 'deleteReusableDelegationSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdsId'
newtype DeleteReusableDelegationSet = DeleteReusableDelegationSet'
    { _drdsId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteReusableDelegationSet' smart constructor.
deleteReusableDelegationSet :: Text -> DeleteReusableDelegationSet
deleteReusableDelegationSet pId =
    DeleteReusableDelegationSet'
    { _drdsId = pId
    }

-- | The ID of the reusable delegation set you want to delete.
drdsId :: Lens' DeleteReusableDelegationSet Text
drdsId = lens _drdsId (\ s a -> s{_drdsId = a});

instance AWSRequest DeleteReusableDelegationSet where
        type Sv DeleteReusableDelegationSet = Route53
        type Rs DeleteReusableDelegationSet =
             DeleteReusableDelegationSetResponse
        request = delete
        response
          = receiveXML
              (\ s h x ->
                 DeleteReusableDelegationSetResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders DeleteReusableDelegationSet where
        toHeaders = const mempty

instance ToPath DeleteReusableDelegationSet where
        toPath DeleteReusableDelegationSet'{..}
          = mconcat
              ["/2013-04-01/delegationset/", toText _drdsId]

instance ToQuery DeleteReusableDelegationSet where
        toQuery = const mempty

-- | Empty response for the request.
--
-- /See:/ 'deleteReusableDelegationSetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdsrStatus'
newtype DeleteReusableDelegationSetResponse = DeleteReusableDelegationSetResponse'
    { _drdsrStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteReusableDelegationSetResponse' smart constructor.
deleteReusableDelegationSetResponse :: Int -> DeleteReusableDelegationSetResponse
deleteReusableDelegationSetResponse pStatus =
    DeleteReusableDelegationSetResponse'
    { _drdsrStatus = pStatus
    }

-- | FIXME: Undocumented member.
drdsrStatus :: Lens' DeleteReusableDelegationSetResponse Int
drdsrStatus = lens _drdsrStatus (\ s a -> s{_drdsrStatus = a});
