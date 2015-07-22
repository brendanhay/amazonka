{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteReusableDelegationSet
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action deletes a reusable delegation set. To delete a reusable
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
    , drdsrqId

    -- * Response
    , DeleteReusableDelegationSetResponse
    -- ** Response constructor
    , deleteReusableDelegationSetResponse
    -- ** Response lenses
    , drdsrsStatus
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
-- * 'drdsrqId'
newtype DeleteReusableDelegationSet = DeleteReusableDelegationSet'
    { _drdsrqId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteReusableDelegationSet' smart constructor.
deleteReusableDelegationSet :: Text -> DeleteReusableDelegationSet
deleteReusableDelegationSet pId_ =
    DeleteReusableDelegationSet'
    { _drdsrqId = pId_
    }

-- | The ID of the reusable delegation set you want to delete.
drdsrqId :: Lens' DeleteReusableDelegationSet Text
drdsrqId = lens _drdsrqId (\ s a -> s{_drdsrqId = a});

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
              ["/2013-04-01/delegationset/", toText _drdsrqId]

instance ToQuery DeleteReusableDelegationSet where
        toQuery = const mempty

-- | Empty response for the request.
--
-- /See:/ 'deleteReusableDelegationSetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdsrsStatus'
newtype DeleteReusableDelegationSetResponse = DeleteReusableDelegationSetResponse'
    { _drdsrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteReusableDelegationSetResponse' smart constructor.
deleteReusableDelegationSetResponse :: Int -> DeleteReusableDelegationSetResponse
deleteReusableDelegationSetResponse pStatus_ =
    DeleteReusableDelegationSetResponse'
    { _drdsrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
drdsrsStatus :: Lens' DeleteReusableDelegationSetResponse Int
drdsrsStatus = lens _drdsrsStatus (\ s a -> s{_drdsrsStatus = a});
