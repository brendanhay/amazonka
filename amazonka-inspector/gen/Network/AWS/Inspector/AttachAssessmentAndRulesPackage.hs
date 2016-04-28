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
-- Module      : Network.AWS.Inspector.AttachAssessmentAndRulesPackage
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the rules package specified by the rules package ARN to the
-- assessment specified by the assessment ARN.
module Network.AWS.Inspector.AttachAssessmentAndRulesPackage
    (
    -- * Creating a Request
      attachAssessmentAndRulesPackage
    , AttachAssessmentAndRulesPackage
    -- * Request Lenses
    , aaarpAssessmentARN
    , aaarpRulesPackageARN

    -- * Destructuring the Response
    , attachAssessmentAndRulesPackageResponse
    , AttachAssessmentAndRulesPackageResponse
    -- * Response Lenses
    , aaarprsMessage
    , aaarprsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachAssessmentAndRulesPackage' smart constructor.
data AttachAssessmentAndRulesPackage = AttachAssessmentAndRulesPackage'
    { _aaarpAssessmentARN   :: !Text
    , _aaarpRulesPackageARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttachAssessmentAndRulesPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaarpAssessmentARN'
--
-- * 'aaarpRulesPackageARN'
attachAssessmentAndRulesPackage
    :: Text -- ^ 'aaarpAssessmentARN'
    -> Text -- ^ 'aaarpRulesPackageARN'
    -> AttachAssessmentAndRulesPackage
attachAssessmentAndRulesPackage pAssessmentARN_ pRulesPackageARN_ =
    AttachAssessmentAndRulesPackage'
    { _aaarpAssessmentARN = pAssessmentARN_
    , _aaarpRulesPackageARN = pRulesPackageARN_
    }

-- | The ARN specifying the assessment to which you want to attach a rules
-- package.
aaarpAssessmentARN :: Lens' AttachAssessmentAndRulesPackage Text
aaarpAssessmentARN = lens _aaarpAssessmentARN (\ s a -> s{_aaarpAssessmentARN = a});

-- | The ARN specifying the rules package that you want to attach to the
-- assessment.
aaarpRulesPackageARN :: Lens' AttachAssessmentAndRulesPackage Text
aaarpRulesPackageARN = lens _aaarpRulesPackageARN (\ s a -> s{_aaarpRulesPackageARN = a});

instance AWSRequest AttachAssessmentAndRulesPackage
         where
        type Rs AttachAssessmentAndRulesPackage =
             AttachAssessmentAndRulesPackageResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 AttachAssessmentAndRulesPackageResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance Hashable AttachAssessmentAndRulesPackage

instance NFData AttachAssessmentAndRulesPackage

instance ToHeaders AttachAssessmentAndRulesPackage
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.AttachAssessmentAndRulesPackage"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AttachAssessmentAndRulesPackage where
        toJSON AttachAssessmentAndRulesPackage'{..}
          = object
              (catMaybes
                 [Just ("assessmentArn" .= _aaarpAssessmentARN),
                  Just ("rulesPackageArn" .= _aaarpRulesPackageARN)])

instance ToPath AttachAssessmentAndRulesPackage where
        toPath = const "/"

instance ToQuery AttachAssessmentAndRulesPackage
         where
        toQuery = const mempty

-- | /See:/ 'attachAssessmentAndRulesPackageResponse' smart constructor.
data AttachAssessmentAndRulesPackageResponse = AttachAssessmentAndRulesPackageResponse'
    { _aaarprsMessage        :: !(Maybe Text)
    , _aaarprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttachAssessmentAndRulesPackageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaarprsMessage'
--
-- * 'aaarprsResponseStatus'
attachAssessmentAndRulesPackageResponse
    :: Int -- ^ 'aaarprsResponseStatus'
    -> AttachAssessmentAndRulesPackageResponse
attachAssessmentAndRulesPackageResponse pResponseStatus_ =
    AttachAssessmentAndRulesPackageResponse'
    { _aaarprsMessage = Nothing
    , _aaarprsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
aaarprsMessage :: Lens' AttachAssessmentAndRulesPackageResponse (Maybe Text)
aaarprsMessage = lens _aaarprsMessage (\ s a -> s{_aaarprsMessage = a});

-- | The response status code.
aaarprsResponseStatus :: Lens' AttachAssessmentAndRulesPackageResponse Int
aaarprsResponseStatus = lens _aaarprsResponseStatus (\ s a -> s{_aaarprsResponseStatus = a});
