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
-- Module      : Network.AWS.Inspector.DeleteAssessmentTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment template that is specified by the ARN of the assessment template.
--
--
module Network.AWS.Inspector.DeleteAssessmentTemplate
    (
    -- * Creating a Request
      deleteAssessmentTemplate
    , DeleteAssessmentTemplate
    -- * Request Lenses
    , datAssessmentTemplateARN

    -- * Destructuring the Response
    , deleteAssessmentTemplateResponse
    , DeleteAssessmentTemplateResponse
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAssessmentTemplate' smart constructor.
newtype DeleteAssessmentTemplate = DeleteAssessmentTemplate'
  { _datAssessmentTemplateARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAssessmentTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datAssessmentTemplateARN' - The ARN that specifies the assessment template that you want to delete.
deleteAssessmentTemplate
    :: Text -- ^ 'datAssessmentTemplateARN'
    -> DeleteAssessmentTemplate
deleteAssessmentTemplate pAssessmentTemplateARN_ =
  DeleteAssessmentTemplate'
    {_datAssessmentTemplateARN = pAssessmentTemplateARN_}


-- | The ARN that specifies the assessment template that you want to delete.
datAssessmentTemplateARN :: Lens' DeleteAssessmentTemplate Text
datAssessmentTemplateARN = lens _datAssessmentTemplateARN (\ s a -> s{_datAssessmentTemplateARN = a})

instance AWSRequest DeleteAssessmentTemplate where
        type Rs DeleteAssessmentTemplate =
             DeleteAssessmentTemplateResponse
        request = postJSON inspector
        response
          = receiveNull DeleteAssessmentTemplateResponse'

instance Hashable DeleteAssessmentTemplate where

instance NFData DeleteAssessmentTemplate where

instance ToHeaders DeleteAssessmentTemplate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DeleteAssessmentTemplate" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAssessmentTemplate where
        toJSON DeleteAssessmentTemplate'{..}
          = object
              (catMaybes
                 [Just
                    ("assessmentTemplateArn" .=
                       _datAssessmentTemplateARN)])

instance ToPath DeleteAssessmentTemplate where
        toPath = const "/"

instance ToQuery DeleteAssessmentTemplate where
        toQuery = const mempty

-- | /See:/ 'deleteAssessmentTemplateResponse' smart constructor.
data DeleteAssessmentTemplateResponse =
  DeleteAssessmentTemplateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAssessmentTemplateResponse' with the minimum fields required to make a request.
--
deleteAssessmentTemplateResponse
    :: DeleteAssessmentTemplateResponse
deleteAssessmentTemplateResponse = DeleteAssessmentTemplateResponse'


instance NFData DeleteAssessmentTemplateResponse
         where
