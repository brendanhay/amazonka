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
-- Module      : Network.AWS.AutoScalingPlans.DeleteScalingPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scaling plan.
--
--
module Network.AWS.AutoScalingPlans.DeleteScalingPlan
    (
    -- * Creating a Request
      deleteScalingPlan
    , DeleteScalingPlan
    -- * Request Lenses
    , dspScalingPlanName
    , dspScalingPlanVersion

    -- * Destructuring the Response
    , deleteScalingPlanResponse
    , DeleteScalingPlanResponse
    -- * Response Lenses
    , dsprsResponseStatus
    ) where

import Network.AWS.AutoScalingPlans.Types
import Network.AWS.AutoScalingPlans.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteScalingPlan' smart constructor.
data DeleteScalingPlan = DeleteScalingPlan'
  { _dspScalingPlanName    :: !Text
  , _dspScalingPlanVersion :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteScalingPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dspScalingPlanName' - The name of the scaling plan.
--
-- * 'dspScalingPlanVersion' - The version of the scaling plan.
deleteScalingPlan
    :: Text -- ^ 'dspScalingPlanName'
    -> Integer -- ^ 'dspScalingPlanVersion'
    -> DeleteScalingPlan
deleteScalingPlan pScalingPlanName_ pScalingPlanVersion_ =
  DeleteScalingPlan'
    { _dspScalingPlanName = pScalingPlanName_
    , _dspScalingPlanVersion = pScalingPlanVersion_
    }


-- | The name of the scaling plan.
dspScalingPlanName :: Lens' DeleteScalingPlan Text
dspScalingPlanName = lens _dspScalingPlanName (\ s a -> s{_dspScalingPlanName = a})

-- | The version of the scaling plan.
dspScalingPlanVersion :: Lens' DeleteScalingPlan Integer
dspScalingPlanVersion = lens _dspScalingPlanVersion (\ s a -> s{_dspScalingPlanVersion = a})

instance AWSRequest DeleteScalingPlan where
        type Rs DeleteScalingPlan = DeleteScalingPlanResponse
        request = postJSON autoScalingPlans
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteScalingPlanResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteScalingPlan where

instance NFData DeleteScalingPlan where

instance ToHeaders DeleteScalingPlan where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleScalingPlannerFrontendService.DeleteScalingPlan"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteScalingPlan where
        toJSON DeleteScalingPlan'{..}
          = object
              (catMaybes
                 [Just ("ScalingPlanName" .= _dspScalingPlanName),
                  Just
                    ("ScalingPlanVersion" .= _dspScalingPlanVersion)])

instance ToPath DeleteScalingPlan where
        toPath = const "/"

instance ToQuery DeleteScalingPlan where
        toQuery = const mempty

-- | /See:/ 'deleteScalingPlanResponse' smart constructor.
newtype DeleteScalingPlanResponse = DeleteScalingPlanResponse'
  { _dsprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteScalingPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsprsResponseStatus' - -- | The response status code.
deleteScalingPlanResponse
    :: Int -- ^ 'dsprsResponseStatus'
    -> DeleteScalingPlanResponse
deleteScalingPlanResponse pResponseStatus_ =
  DeleteScalingPlanResponse' {_dsprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsprsResponseStatus :: Lens' DeleteScalingPlanResponse Int
dsprsResponseStatus = lens _dsprsResponseStatus (\ s a -> s{_dsprsResponseStatus = a})

instance NFData DeleteScalingPlanResponse where
