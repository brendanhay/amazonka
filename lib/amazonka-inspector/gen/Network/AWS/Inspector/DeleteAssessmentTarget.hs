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
-- Module      : Network.AWS.Inspector.DeleteAssessmentTarget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment target that is specified by the ARN of the assessment target.
--
--
module Network.AWS.Inspector.DeleteAssessmentTarget
    (
    -- * Creating a Request
      deleteAssessmentTarget
    , DeleteAssessmentTarget
    -- * Request Lenses
    , datAssessmentTargetARN

    -- * Destructuring the Response
    , deleteAssessmentTargetResponse
    , DeleteAssessmentTargetResponse
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAssessmentTarget' smart constructor.
newtype DeleteAssessmentTarget = DeleteAssessmentTarget'
  { _datAssessmentTargetARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAssessmentTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datAssessmentTargetARN' - The ARN that specifies the assessment target that you want to delete.
deleteAssessmentTarget
    :: Text -- ^ 'datAssessmentTargetARN'
    -> DeleteAssessmentTarget
deleteAssessmentTarget pAssessmentTargetARN_ =
  DeleteAssessmentTarget' {_datAssessmentTargetARN = pAssessmentTargetARN_}


-- | The ARN that specifies the assessment target that you want to delete.
datAssessmentTargetARN :: Lens' DeleteAssessmentTarget Text
datAssessmentTargetARN = lens _datAssessmentTargetARN (\ s a -> s{_datAssessmentTargetARN = a})

instance AWSRequest DeleteAssessmentTarget where
        type Rs DeleteAssessmentTarget =
             DeleteAssessmentTargetResponse
        request = postJSON inspector
        response
          = receiveNull DeleteAssessmentTargetResponse'

instance Hashable DeleteAssessmentTarget where

instance NFData DeleteAssessmentTarget where

instance ToHeaders DeleteAssessmentTarget where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DeleteAssessmentTarget" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAssessmentTarget where
        toJSON DeleteAssessmentTarget'{..}
          = object
              (catMaybes
                 [Just
                    ("assessmentTargetArn" .= _datAssessmentTargetARN)])

instance ToPath DeleteAssessmentTarget where
        toPath = const "/"

instance ToQuery DeleteAssessmentTarget where
        toQuery = const mempty

-- | /See:/ 'deleteAssessmentTargetResponse' smart constructor.
data DeleteAssessmentTargetResponse =
  DeleteAssessmentTargetResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAssessmentTargetResponse' with the minimum fields required to make a request.
--
deleteAssessmentTargetResponse
    :: DeleteAssessmentTargetResponse
deleteAssessmentTargetResponse = DeleteAssessmentTargetResponse'


instance NFData DeleteAssessmentTargetResponse where
