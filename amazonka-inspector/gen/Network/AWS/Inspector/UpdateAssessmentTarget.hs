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
-- Module      : Network.AWS.Inspector.UpdateAssessmentTarget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the assessment target that is specified by the ARN of the assessment target.
--
--
module Network.AWS.Inspector.UpdateAssessmentTarget
    (
    -- * Creating a Request
      updateAssessmentTarget
    , UpdateAssessmentTarget
    -- * Request Lenses
    , uatAssessmentTargetARN
    , uatAssessmentTargetName
    , uatResourceGroupARN

    -- * Destructuring the Response
    , updateAssessmentTargetResponse
    , UpdateAssessmentTargetResponse
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAssessmentTarget' smart constructor.
data UpdateAssessmentTarget = UpdateAssessmentTarget'
  { _uatAssessmentTargetARN  :: !Text
  , _uatAssessmentTargetName :: !Text
  , _uatResourceGroupARN     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAssessmentTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uatAssessmentTargetARN' - The ARN of the assessment target that you want to update.
--
-- * 'uatAssessmentTargetName' - The name of the assessment target that you want to update.
--
-- * 'uatResourceGroupARN' - The ARN of the resource group that is used to specify the new resource group to associate with the assessment target.
updateAssessmentTarget
    :: Text -- ^ 'uatAssessmentTargetARN'
    -> Text -- ^ 'uatAssessmentTargetName'
    -> Text -- ^ 'uatResourceGroupARN'
    -> UpdateAssessmentTarget
updateAssessmentTarget pAssessmentTargetARN_ pAssessmentTargetName_ pResourceGroupARN_ =
  UpdateAssessmentTarget'
    { _uatAssessmentTargetARN = pAssessmentTargetARN_
    , _uatAssessmentTargetName = pAssessmentTargetName_
    , _uatResourceGroupARN = pResourceGroupARN_
    }


-- | The ARN of the assessment target that you want to update.
uatAssessmentTargetARN :: Lens' UpdateAssessmentTarget Text
uatAssessmentTargetARN = lens _uatAssessmentTargetARN (\ s a -> s{_uatAssessmentTargetARN = a})

-- | The name of the assessment target that you want to update.
uatAssessmentTargetName :: Lens' UpdateAssessmentTarget Text
uatAssessmentTargetName = lens _uatAssessmentTargetName (\ s a -> s{_uatAssessmentTargetName = a})

-- | The ARN of the resource group that is used to specify the new resource group to associate with the assessment target.
uatResourceGroupARN :: Lens' UpdateAssessmentTarget Text
uatResourceGroupARN = lens _uatResourceGroupARN (\ s a -> s{_uatResourceGroupARN = a})

instance AWSRequest UpdateAssessmentTarget where
        type Rs UpdateAssessmentTarget =
             UpdateAssessmentTargetResponse
        request = postJSON inspector
        response
          = receiveNull UpdateAssessmentTargetResponse'

instance Hashable UpdateAssessmentTarget where

instance NFData UpdateAssessmentTarget where

instance ToHeaders UpdateAssessmentTarget where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.UpdateAssessmentTarget" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAssessmentTarget where
        toJSON UpdateAssessmentTarget'{..}
          = object
              (catMaybes
                 [Just
                    ("assessmentTargetArn" .= _uatAssessmentTargetARN),
                  Just
                    ("assessmentTargetName" .= _uatAssessmentTargetName),
                  Just ("resourceGroupArn" .= _uatResourceGroupARN)])

instance ToPath UpdateAssessmentTarget where
        toPath = const "/"

instance ToQuery UpdateAssessmentTarget where
        toQuery = const mempty

-- | /See:/ 'updateAssessmentTargetResponse' smart constructor.
data UpdateAssessmentTargetResponse =
  UpdateAssessmentTargetResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAssessmentTargetResponse' with the minimum fields required to make a request.
--
updateAssessmentTargetResponse
    :: UpdateAssessmentTargetResponse
updateAssessmentTargetResponse = UpdateAssessmentTargetResponse'


instance NFData UpdateAssessmentTargetResponse where
