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
-- Module      : Network.AWS.SES.DeleteTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an email template.
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.DeleteTemplate
    (
    -- * Creating a Request
      deleteTemplate
    , DeleteTemplate
    -- * Request Lenses
    , dtTemplateName

    -- * Destructuring the Response
    , deleteTemplateResponse
    , DeleteTemplateResponse
    -- * Response Lenses
    , dtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to delete an email template. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'deleteTemplate' smart constructor.
newtype DeleteTemplate = DeleteTemplate'
  { _dtTemplateName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTemplateName' - The name of the template to be deleted.
deleteTemplate
    :: Text -- ^ 'dtTemplateName'
    -> DeleteTemplate
deleteTemplate pTemplateName_ =
  DeleteTemplate' {_dtTemplateName = pTemplateName_}


-- | The name of the template to be deleted.
dtTemplateName :: Lens' DeleteTemplate Text
dtTemplateName = lens _dtTemplateName (\ s a -> s{_dtTemplateName = a})

instance AWSRequest DeleteTemplate where
        type Rs DeleteTemplate = DeleteTemplateResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "DeleteTemplateResult"
              (\ s h x ->
                 DeleteTemplateResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteTemplate where

instance NFData DeleteTemplate where

instance ToHeaders DeleteTemplate where
        toHeaders = const mempty

instance ToPath DeleteTemplate where
        toPath = const "/"

instance ToQuery DeleteTemplate where
        toQuery DeleteTemplate'{..}
          = mconcat
              ["Action" =: ("DeleteTemplate" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TemplateName" =: _dtTemplateName]

-- | /See:/ 'deleteTemplateResponse' smart constructor.
newtype DeleteTemplateResponse = DeleteTemplateResponse'
  { _dtrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsResponseStatus' - -- | The response status code.
deleteTemplateResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DeleteTemplateResponse
deleteTemplateResponse pResponseStatus_ =
  DeleteTemplateResponse' {_dtrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dtrsResponseStatus :: Lens' DeleteTemplateResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

instance NFData DeleteTemplateResponse where
