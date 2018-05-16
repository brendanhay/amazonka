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
-- Module      : Network.AWS.Inspector.DeleteAssessmentRun
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment run that is specified by the ARN of the assessment run.
--
--
module Network.AWS.Inspector.DeleteAssessmentRun
    (
    -- * Creating a Request
      deleteAssessmentRun
    , DeleteAssessmentRun
    -- * Request Lenses
    , darAssessmentRunARN

    -- * Destructuring the Response
    , deleteAssessmentRunResponse
    , DeleteAssessmentRunResponse
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAssessmentRun' smart constructor.
newtype DeleteAssessmentRun = DeleteAssessmentRun'
  { _darAssessmentRunARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAssessmentRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darAssessmentRunARN' - The ARN that specifies the assessment run that you want to delete.
deleteAssessmentRun
    :: Text -- ^ 'darAssessmentRunARN'
    -> DeleteAssessmentRun
deleteAssessmentRun pAssessmentRunARN_ =
  DeleteAssessmentRun' {_darAssessmentRunARN = pAssessmentRunARN_}


-- | The ARN that specifies the assessment run that you want to delete.
darAssessmentRunARN :: Lens' DeleteAssessmentRun Text
darAssessmentRunARN = lens _darAssessmentRunARN (\ s a -> s{_darAssessmentRunARN = a})

instance AWSRequest DeleteAssessmentRun where
        type Rs DeleteAssessmentRun =
             DeleteAssessmentRunResponse
        request = postJSON inspector
        response = receiveNull DeleteAssessmentRunResponse'

instance Hashable DeleteAssessmentRun where

instance NFData DeleteAssessmentRun where

instance ToHeaders DeleteAssessmentRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DeleteAssessmentRun" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAssessmentRun where
        toJSON DeleteAssessmentRun'{..}
          = object
              (catMaybes
                 [Just ("assessmentRunArn" .= _darAssessmentRunARN)])

instance ToPath DeleteAssessmentRun where
        toPath = const "/"

instance ToQuery DeleteAssessmentRun where
        toQuery = const mempty

-- | /See:/ 'deleteAssessmentRunResponse' smart constructor.
data DeleteAssessmentRunResponse =
  DeleteAssessmentRunResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAssessmentRunResponse' with the minimum fields required to make a request.
--
deleteAssessmentRunResponse
    :: DeleteAssessmentRunResponse
deleteAssessmentRunResponse = DeleteAssessmentRunResponse'


instance NFData DeleteAssessmentRunResponse where
