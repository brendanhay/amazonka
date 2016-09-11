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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reusable delegation set. Send a 'DELETE' request to the '\/2013-04-01\/delegationset\/delegation set ID ' resource.
--
-- You can delete a reusable delegation set only if there are no associated hosted zones.
--
-- To verify that the reusable delegation set is not associated with any hosted zones, run the < GetReusableDelegationSet> action and specify the ID of the reusable delegation set that you want to delete.
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
    , drdsrsResponseStatus
    ) where

import           Network.AWS.Lens
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
        type Rs DeleteReusableDelegationSet =
             DeleteReusableDelegationSetResponse
        request = delete route53
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteReusableDelegationSetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteReusableDelegationSet

instance NFData DeleteReusableDelegationSet

instance ToHeaders DeleteReusableDelegationSet where
        toHeaders = const mempty

instance ToPath DeleteReusableDelegationSet where
        toPath DeleteReusableDelegationSet'{..}
          = mconcat
              ["/2013-04-01/delegationset/", toBS _drdsId]

instance ToQuery DeleteReusableDelegationSet where
        toQuery = const mempty

-- | An empty element.
--
-- /See:/ 'deleteReusableDelegationSetResponse' smart constructor.
newtype DeleteReusableDelegationSetResponse = DeleteReusableDelegationSetResponse'
    { _drdsrsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteReusableDelegationSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdsrsResponseStatus'
deleteReusableDelegationSetResponse
    :: Int -- ^ 'drdsrsResponseStatus'
    -> DeleteReusableDelegationSetResponse
deleteReusableDelegationSetResponse pResponseStatus_ =
    DeleteReusableDelegationSetResponse'
    { _drdsrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
drdsrsResponseStatus :: Lens' DeleteReusableDelegationSetResponse Int
drdsrsResponseStatus = lens _drdsrsResponseStatus (\ s a -> s{_drdsrsResponseStatus = a});

instance NFData DeleteReusableDelegationSetResponse
