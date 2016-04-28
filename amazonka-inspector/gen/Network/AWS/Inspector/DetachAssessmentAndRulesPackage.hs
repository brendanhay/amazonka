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
-- Module      : Network.AWS.Inspector.DetachAssessmentAndRulesPackage
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the rules package specified by the rules package ARN from the
-- assessment specified by the assessment ARN.
module Network.AWS.Inspector.DetachAssessmentAndRulesPackage
    (
    -- * Creating a Request
      detachAssessmentAndRulesPackage
    , DetachAssessmentAndRulesPackage
    -- * Request Lenses
    , daarpAssessmentARN
    , daarpRulesPackageARN

    -- * Destructuring the Response
    , detachAssessmentAndRulesPackageResponse
    , DetachAssessmentAndRulesPackageResponse
    -- * Response Lenses
    , daarprsMessage
    , daarprsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachAssessmentAndRulesPackage' smart constructor.
data DetachAssessmentAndRulesPackage = DetachAssessmentAndRulesPackage'
    { _daarpAssessmentARN   :: !Text
    , _daarpRulesPackageARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachAssessmentAndRulesPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarpAssessmentARN'
--
-- * 'daarpRulesPackageARN'
detachAssessmentAndRulesPackage
    :: Text -- ^ 'daarpAssessmentARN'
    -> Text -- ^ 'daarpRulesPackageARN'
    -> DetachAssessmentAndRulesPackage
detachAssessmentAndRulesPackage pAssessmentARN_ pRulesPackageARN_ =
    DetachAssessmentAndRulesPackage'
    { _daarpAssessmentARN = pAssessmentARN_
    , _daarpRulesPackageARN = pRulesPackageARN_
    }

-- | The ARN specifying the assessment from which you want to detach a rules
-- package.
daarpAssessmentARN :: Lens' DetachAssessmentAndRulesPackage Text
daarpAssessmentARN = lens _daarpAssessmentARN (\ s a -> s{_daarpAssessmentARN = a});

-- | The ARN specifying the rules package that you want to detach from the
-- assessment.
daarpRulesPackageARN :: Lens' DetachAssessmentAndRulesPackage Text
daarpRulesPackageARN = lens _daarpRulesPackageARN (\ s a -> s{_daarpRulesPackageARN = a});

instance AWSRequest DetachAssessmentAndRulesPackage
         where
        type Rs DetachAssessmentAndRulesPackage =
             DetachAssessmentAndRulesPackageResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DetachAssessmentAndRulesPackageResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance Hashable DetachAssessmentAndRulesPackage

instance NFData DetachAssessmentAndRulesPackage

instance ToHeaders DetachAssessmentAndRulesPackage
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DetachAssessmentAndRulesPackage"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetachAssessmentAndRulesPackage where
        toJSON DetachAssessmentAndRulesPackage'{..}
          = object
              (catMaybes
                 [Just ("assessmentArn" .= _daarpAssessmentARN),
                  Just ("rulesPackageArn" .= _daarpRulesPackageARN)])

instance ToPath DetachAssessmentAndRulesPackage where
        toPath = const "/"

instance ToQuery DetachAssessmentAndRulesPackage
         where
        toQuery = const mempty

-- | /See:/ 'detachAssessmentAndRulesPackageResponse' smart constructor.
data DetachAssessmentAndRulesPackageResponse = DetachAssessmentAndRulesPackageResponse'
    { _daarprsMessage        :: !(Maybe Text)
    , _daarprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachAssessmentAndRulesPackageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarprsMessage'
--
-- * 'daarprsResponseStatus'
detachAssessmentAndRulesPackageResponse
    :: Int -- ^ 'daarprsResponseStatus'
    -> DetachAssessmentAndRulesPackageResponse
detachAssessmentAndRulesPackageResponse pResponseStatus_ =
    DetachAssessmentAndRulesPackageResponse'
    { _daarprsMessage = Nothing
    , _daarprsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
daarprsMessage :: Lens' DetachAssessmentAndRulesPackageResponse (Maybe Text)
daarprsMessage = lens _daarprsMessage (\ s a -> s{_daarprsMessage = a});

-- | The response status code.
daarprsResponseStatus :: Lens' DetachAssessmentAndRulesPackageResponse Int
daarprsResponseStatus = lens _daarprsResponseStatus (\ s a -> s{_daarprsResponseStatus = a});
