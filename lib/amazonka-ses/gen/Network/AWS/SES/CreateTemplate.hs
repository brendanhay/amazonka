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
-- Module      : Network.AWS.SES.CreateTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an email template. Email templates enable you to send personalized email to one or more destinations in a single API operation. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.CreateTemplate
    (
    -- * Creating a Request
      createTemplate
    , CreateTemplate
    -- * Request Lenses
    , ctTemplate

    -- * Destructuring the Response
    , createTemplateResponse
    , CreateTemplateResponse
    -- * Response Lenses
    , ctrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to create an email template. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'createTemplate' smart constructor.
newtype CreateTemplate = CreateTemplate'
  { _ctTemplate :: Template
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctTemplate' - The content of the email, composed of a subject line, an HTML part, and a text-only part.
createTemplate
    :: Template -- ^ 'ctTemplate'
    -> CreateTemplate
createTemplate pTemplate_ = CreateTemplate' {_ctTemplate = pTemplate_}


-- | The content of the email, composed of a subject line, an HTML part, and a text-only part.
ctTemplate :: Lens' CreateTemplate Template
ctTemplate = lens _ctTemplate (\ s a -> s{_ctTemplate = a})

instance AWSRequest CreateTemplate where
        type Rs CreateTemplate = CreateTemplateResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "CreateTemplateResult"
              (\ s h x ->
                 CreateTemplateResponse' <$> (pure (fromEnum s)))

instance Hashable CreateTemplate where

instance NFData CreateTemplate where

instance ToHeaders CreateTemplate where
        toHeaders = const mempty

instance ToPath CreateTemplate where
        toPath = const "/"

instance ToQuery CreateTemplate where
        toQuery CreateTemplate'{..}
          = mconcat
              ["Action" =: ("CreateTemplate" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Template" =: _ctTemplate]

-- | /See:/ 'createTemplateResponse' smart constructor.
newtype CreateTemplateResponse = CreateTemplateResponse'
  { _ctrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createTemplateResponse
    :: Int -- ^ 'ctrsResponseStatus'
    -> CreateTemplateResponse
createTemplateResponse pResponseStatus_ =
  CreateTemplateResponse' {_ctrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateTemplateResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\ s a -> s{_ctrsResponseStatus = a})

instance NFData CreateTemplateResponse where
