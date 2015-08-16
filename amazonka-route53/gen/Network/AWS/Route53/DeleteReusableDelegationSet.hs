{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteReusableDelegationSet
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action deletes a reusable delegation set. To delete a reusable
-- delegation set, send a 'DELETE' request to the
-- '2013-04-01\/delegationset\/delegation set ID' resource.
--
-- You can delete a reusable delegation set only if there are no associated
-- hosted zones. If your reusable delegation set contains associated hosted
-- zones, you must delete them before you can delete your reusable
-- delegation set. If you try to delete a reusable delegation set that
-- contains associated hosted zones, Route 53 will deny your request with a
-- 'DelegationSetInUse' error.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteReusableDelegationSet.html AWS API Reference> for DeleteReusableDelegationSet.
module Network.AWS.Route53.DeleteReusableDelegationSet
    (
    -- * Creating a Request
      deleteReusableDelegationSet
    , DeleteReusableDelegationSet
    -- * Request Lenses
    , drdsId

    -- * Destructuring the Response
    , deleteReusableDelegationSetResponse
    , DeleteReusableDelegationSetResponse
    -- * Response Lenses
    , drdsrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type containing the information for the delete request.
--
-- /See:/ 'deleteReusableDelegationSet' smart constructor.
newtype DeleteReusableDelegationSet = DeleteReusableDelegationSet'
    { _drdsId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteReusableDelegationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdsId'
deleteReusableDelegationSet
    :: Text -- ^ 'drdsId'
    -> DeleteReusableDelegationSet
deleteReusableDelegationSet pId_ =
    DeleteReusableDelegationSet'
    { _drdsId = pId_
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
          = receiveEmpty
              (\ s h x ->
                 DeleteReusableDelegationSetResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders DeleteReusableDelegationSet where
        toHeaders = const mempty

instance ToPath DeleteReusableDelegationSet where
        toPath DeleteReusableDelegationSet'{..}
          = mconcat
              ["/2013-04-01/delegationset/", toBS _drdsId]

instance ToQuery DeleteReusableDelegationSet where
        toQuery = const mempty

-- | Empty response for the request.
--
-- /See:/ 'deleteReusableDelegationSetResponse' smart constructor.
newtype DeleteReusableDelegationSetResponse = DeleteReusableDelegationSetResponse'
    { _drdsrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteReusableDelegationSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdsrsStatus'
deleteReusableDelegationSetResponse
    :: Int -- ^ 'drdsrsStatus'
    -> DeleteReusableDelegationSetResponse
deleteReusableDelegationSetResponse pStatus_ =
    DeleteReusableDelegationSetResponse'
    { _drdsrsStatus = pStatus_
    }

-- | The response status code.
drdsrsStatus :: Lens' DeleteReusableDelegationSetResponse Int
drdsrsStatus = lens _drdsrsStatus (\ s a -> s{_drdsrsStatus = a});
