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
-- Module      : Network.AWS.AutoScalingPlans.UpdateScalingPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the scaling plan for the specified scaling plan.
--
--
-- You cannot update a scaling plan if it is in the process of being created, updated, or deleted.
--
module Network.AWS.AutoScalingPlans.UpdateScalingPlan
    (
    -- * Creating a Request
      updateScalingPlan
    , UpdateScalingPlan
    -- * Request Lenses
    , uspScalingInstructions
    , uspApplicationSource
    , uspScalingPlanName
    , uspScalingPlanVersion

    -- * Destructuring the Response
    , updateScalingPlanResponse
    , UpdateScalingPlanResponse
    -- * Response Lenses
    , usprsResponseStatus
    ) where

import Network.AWS.AutoScalingPlans.Types
import Network.AWS.AutoScalingPlans.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateScalingPlan' smart constructor.
data UpdateScalingPlan = UpdateScalingPlan'
  { _uspScalingInstructions :: !(Maybe [ScalingInstruction])
  , _uspApplicationSource   :: !(Maybe ApplicationSource)
  , _uspScalingPlanName     :: !Text
  , _uspScalingPlanVersion  :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateScalingPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uspScalingInstructions' - The scaling instructions.
--
-- * 'uspApplicationSource' - A CloudFormation stack or set of tags.
--
-- * 'uspScalingPlanName' - The name of the scaling plan.
--
-- * 'uspScalingPlanVersion' - The version number.
updateScalingPlan
    :: Text -- ^ 'uspScalingPlanName'
    -> Integer -- ^ 'uspScalingPlanVersion'
    -> UpdateScalingPlan
updateScalingPlan pScalingPlanName_ pScalingPlanVersion_ =
  UpdateScalingPlan'
    { _uspScalingInstructions = Nothing
    , _uspApplicationSource = Nothing
    , _uspScalingPlanName = pScalingPlanName_
    , _uspScalingPlanVersion = pScalingPlanVersion_
    }


-- | The scaling instructions.
uspScalingInstructions :: Lens' UpdateScalingPlan [ScalingInstruction]
uspScalingInstructions = lens _uspScalingInstructions (\ s a -> s{_uspScalingInstructions = a}) . _Default . _Coerce

-- | A CloudFormation stack or set of tags.
uspApplicationSource :: Lens' UpdateScalingPlan (Maybe ApplicationSource)
uspApplicationSource = lens _uspApplicationSource (\ s a -> s{_uspApplicationSource = a})

-- | The name of the scaling plan.
uspScalingPlanName :: Lens' UpdateScalingPlan Text
uspScalingPlanName = lens _uspScalingPlanName (\ s a -> s{_uspScalingPlanName = a})

-- | The version number.
uspScalingPlanVersion :: Lens' UpdateScalingPlan Integer
uspScalingPlanVersion = lens _uspScalingPlanVersion (\ s a -> s{_uspScalingPlanVersion = a})

instance AWSRequest UpdateScalingPlan where
        type Rs UpdateScalingPlan = UpdateScalingPlanResponse
        request = postJSON autoScalingPlans
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateScalingPlanResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateScalingPlan where

instance NFData UpdateScalingPlan where

instance ToHeaders UpdateScalingPlan where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleScalingPlannerFrontendService.UpdateScalingPlan"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateScalingPlan where
        toJSON UpdateScalingPlan'{..}
          = object
              (catMaybes
                 [("ScalingInstructions" .=) <$>
                    _uspScalingInstructions,
                  ("ApplicationSource" .=) <$> _uspApplicationSource,
                  Just ("ScalingPlanName" .= _uspScalingPlanName),
                  Just
                    ("ScalingPlanVersion" .= _uspScalingPlanVersion)])

instance ToPath UpdateScalingPlan where
        toPath = const "/"

instance ToQuery UpdateScalingPlan where
        toQuery = const mempty

-- | /See:/ 'updateScalingPlanResponse' smart constructor.
newtype UpdateScalingPlanResponse = UpdateScalingPlanResponse'
  { _usprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateScalingPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usprsResponseStatus' - -- | The response status code.
updateScalingPlanResponse
    :: Int -- ^ 'usprsResponseStatus'
    -> UpdateScalingPlanResponse
updateScalingPlanResponse pResponseStatus_ =
  UpdateScalingPlanResponse' {_usprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
usprsResponseStatus :: Lens' UpdateScalingPlanResponse Int
usprsResponseStatus = lens _usprsResponseStatus (\ s a -> s{_usprsResponseStatus = a})

instance NFData UpdateScalingPlanResponse where
