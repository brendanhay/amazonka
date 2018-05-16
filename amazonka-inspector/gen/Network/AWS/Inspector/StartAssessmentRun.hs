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
-- Module      : Network.AWS.Inspector.StartAssessmentRun
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the assessment run specified by the ARN of the assessment template. For this API to function properly, you must not exceed the limit of running up to 500 concurrent agents per AWS account.
--
--
module Network.AWS.Inspector.StartAssessmentRun
    (
    -- * Creating a Request
      startAssessmentRun
    , StartAssessmentRun
    -- * Request Lenses
    , sarAssessmentRunName
    , sarAssessmentTemplateARN

    -- * Destructuring the Response
    , startAssessmentRunResponse
    , StartAssessmentRunResponse
    -- * Response Lenses
    , sarrsResponseStatus
    , sarrsAssessmentRunARN
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startAssessmentRun' smart constructor.
data StartAssessmentRun = StartAssessmentRun'
  { _sarAssessmentRunName     :: !(Maybe Text)
  , _sarAssessmentTemplateARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartAssessmentRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sarAssessmentRunName' - You can specify the name for the assessment run. The name must be unique for the assessment template whose ARN is used to start the assessment run.
--
-- * 'sarAssessmentTemplateARN' - The ARN of the assessment template of the assessment run that you want to start.
startAssessmentRun
    :: Text -- ^ 'sarAssessmentTemplateARN'
    -> StartAssessmentRun
startAssessmentRun pAssessmentTemplateARN_ =
  StartAssessmentRun'
    { _sarAssessmentRunName = Nothing
    , _sarAssessmentTemplateARN = pAssessmentTemplateARN_
    }


-- | You can specify the name for the assessment run. The name must be unique for the assessment template whose ARN is used to start the assessment run.
sarAssessmentRunName :: Lens' StartAssessmentRun (Maybe Text)
sarAssessmentRunName = lens _sarAssessmentRunName (\ s a -> s{_sarAssessmentRunName = a})

-- | The ARN of the assessment template of the assessment run that you want to start.
sarAssessmentTemplateARN :: Lens' StartAssessmentRun Text
sarAssessmentTemplateARN = lens _sarAssessmentTemplateARN (\ s a -> s{_sarAssessmentTemplateARN = a})

instance AWSRequest StartAssessmentRun where
        type Rs StartAssessmentRun =
             StartAssessmentRunResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 StartAssessmentRunResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "assessmentRunArn"))

instance Hashable StartAssessmentRun where

instance NFData StartAssessmentRun where

instance ToHeaders StartAssessmentRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.StartAssessmentRun" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartAssessmentRun where
        toJSON StartAssessmentRun'{..}
          = object
              (catMaybes
                 [("assessmentRunName" .=) <$> _sarAssessmentRunName,
                  Just
                    ("assessmentTemplateArn" .=
                       _sarAssessmentTemplateARN)])

instance ToPath StartAssessmentRun where
        toPath = const "/"

instance ToQuery StartAssessmentRun where
        toQuery = const mempty

-- | /See:/ 'startAssessmentRunResponse' smart constructor.
data StartAssessmentRunResponse = StartAssessmentRunResponse'
  { _sarrsResponseStatus   :: !Int
  , _sarrsAssessmentRunARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartAssessmentRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sarrsResponseStatus' - -- | The response status code.
--
-- * 'sarrsAssessmentRunARN' - The ARN of the assessment run that has been started.
startAssessmentRunResponse
    :: Int -- ^ 'sarrsResponseStatus'
    -> Text -- ^ 'sarrsAssessmentRunARN'
    -> StartAssessmentRunResponse
startAssessmentRunResponse pResponseStatus_ pAssessmentRunARN_ =
  StartAssessmentRunResponse'
    { _sarrsResponseStatus = pResponseStatus_
    , _sarrsAssessmentRunARN = pAssessmentRunARN_
    }


-- | -- | The response status code.
sarrsResponseStatus :: Lens' StartAssessmentRunResponse Int
sarrsResponseStatus = lens _sarrsResponseStatus (\ s a -> s{_sarrsResponseStatus = a})

-- | The ARN of the assessment run that has been started.
sarrsAssessmentRunARN :: Lens' StartAssessmentRunResponse Text
sarrsAssessmentRunARN = lens _sarrsAssessmentRunARN (\ s a -> s{_sarrsAssessmentRunARN = a})

instance NFData StartAssessmentRunResponse where
