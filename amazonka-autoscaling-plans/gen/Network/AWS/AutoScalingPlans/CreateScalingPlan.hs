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
-- Module      : Network.AWS.AutoScalingPlans.CreateScalingPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scaling plan.
--
--
-- A scaling plan contains a set of instructions used to configure dynamic scaling for the scalable resources in your application. AWS Auto Scaling creates target tracking scaling policies based on the scaling instructions in your scaling plan.
--
module Network.AWS.AutoScalingPlans.CreateScalingPlan
    (
    -- * Creating a Request
      createScalingPlan
    , CreateScalingPlan
    -- * Request Lenses
    , cspScalingPlanName
    , cspApplicationSource
    , cspScalingInstructions

    -- * Destructuring the Response
    , createScalingPlanResponse
    , CreateScalingPlanResponse
    -- * Response Lenses
    , csprsResponseStatus
    , csprsScalingPlanVersion
    ) where

import Network.AWS.AutoScalingPlans.Types
import Network.AWS.AutoScalingPlans.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createScalingPlan' smart constructor.
data CreateScalingPlan = CreateScalingPlan'
  { _cspScalingPlanName     :: !Text
  , _cspApplicationSource   :: !ApplicationSource
  , _cspScalingInstructions :: ![ScalingInstruction]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateScalingPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cspScalingPlanName' - The name of the scaling plan. Names cannot contain vertical bars, colons, or forward slashes.
--
-- * 'cspApplicationSource' - A CloudFormation stack or set of tags. You can create one scaling plan per application source.
--
-- * 'cspScalingInstructions' - The scaling instructions.
createScalingPlan
    :: Text -- ^ 'cspScalingPlanName'
    -> ApplicationSource -- ^ 'cspApplicationSource'
    -> CreateScalingPlan
createScalingPlan pScalingPlanName_ pApplicationSource_ =
  CreateScalingPlan'
    { _cspScalingPlanName = pScalingPlanName_
    , _cspApplicationSource = pApplicationSource_
    , _cspScalingInstructions = mempty
    }


-- | The name of the scaling plan. Names cannot contain vertical bars, colons, or forward slashes.
cspScalingPlanName :: Lens' CreateScalingPlan Text
cspScalingPlanName = lens _cspScalingPlanName (\ s a -> s{_cspScalingPlanName = a})

-- | A CloudFormation stack or set of tags. You can create one scaling plan per application source.
cspApplicationSource :: Lens' CreateScalingPlan ApplicationSource
cspApplicationSource = lens _cspApplicationSource (\ s a -> s{_cspApplicationSource = a})

-- | The scaling instructions.
cspScalingInstructions :: Lens' CreateScalingPlan [ScalingInstruction]
cspScalingInstructions = lens _cspScalingInstructions (\ s a -> s{_cspScalingInstructions = a}) . _Coerce

instance AWSRequest CreateScalingPlan where
        type Rs CreateScalingPlan = CreateScalingPlanResponse
        request = postJSON autoScalingPlans
        response
          = receiveJSON
              (\ s h x ->
                 CreateScalingPlanResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "ScalingPlanVersion"))

instance Hashable CreateScalingPlan where

instance NFData CreateScalingPlan where

instance ToHeaders CreateScalingPlan where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleScalingPlannerFrontendService.CreateScalingPlan"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateScalingPlan where
        toJSON CreateScalingPlan'{..}
          = object
              (catMaybes
                 [Just ("ScalingPlanName" .= _cspScalingPlanName),
                  Just ("ApplicationSource" .= _cspApplicationSource),
                  Just
                    ("ScalingInstructions" .= _cspScalingInstructions)])

instance ToPath CreateScalingPlan where
        toPath = const "/"

instance ToQuery CreateScalingPlan where
        toQuery = const mempty

-- | /See:/ 'createScalingPlanResponse' smart constructor.
data CreateScalingPlanResponse = CreateScalingPlanResponse'
  { _csprsResponseStatus     :: !Int
  , _csprsScalingPlanVersion :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateScalingPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csprsResponseStatus' - -- | The response status code.
--
-- * 'csprsScalingPlanVersion' - The version of the scaling plan. This value is always 1.
createScalingPlanResponse
    :: Int -- ^ 'csprsResponseStatus'
    -> Integer -- ^ 'csprsScalingPlanVersion'
    -> CreateScalingPlanResponse
createScalingPlanResponse pResponseStatus_ pScalingPlanVersion_ =
  CreateScalingPlanResponse'
    { _csprsResponseStatus = pResponseStatus_
    , _csprsScalingPlanVersion = pScalingPlanVersion_
    }


-- | -- | The response status code.
csprsResponseStatus :: Lens' CreateScalingPlanResponse Int
csprsResponseStatus = lens _csprsResponseStatus (\ s a -> s{_csprsResponseStatus = a})

-- | The version of the scaling plan. This value is always 1.
csprsScalingPlanVersion :: Lens' CreateScalingPlanResponse Integer
csprsScalingPlanVersion = lens _csprsScalingPlanVersion (\ s a -> s{_csprsScalingPlanVersion = a})

instance NFData CreateScalingPlanResponse where
