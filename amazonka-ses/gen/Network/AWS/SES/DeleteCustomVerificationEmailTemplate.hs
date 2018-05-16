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
-- Module      : Network.AWS.SES.DeleteCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing custom verification email template.
--
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.DeleteCustomVerificationEmailTemplate
    (
    -- * Creating a Request
      deleteCustomVerificationEmailTemplate
    , DeleteCustomVerificationEmailTemplate
    -- * Request Lenses
    , dcvetTemplateName

    -- * Destructuring the Response
    , deleteCustomVerificationEmailTemplateResponse
    , DeleteCustomVerificationEmailTemplateResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to delete an existing custom verification email template.
--
--
--
-- /See:/ 'deleteCustomVerificationEmailTemplate' smart constructor.
newtype DeleteCustomVerificationEmailTemplate = DeleteCustomVerificationEmailTemplate'
  { _dcvetTemplateName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCustomVerificationEmailTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvetTemplateName' - The name of the custom verification email template that you want to delete.
deleteCustomVerificationEmailTemplate
    :: Text -- ^ 'dcvetTemplateName'
    -> DeleteCustomVerificationEmailTemplate
deleteCustomVerificationEmailTemplate pTemplateName_ =
  DeleteCustomVerificationEmailTemplate' {_dcvetTemplateName = pTemplateName_}


-- | The name of the custom verification email template that you want to delete.
dcvetTemplateName :: Lens' DeleteCustomVerificationEmailTemplate Text
dcvetTemplateName = lens _dcvetTemplateName (\ s a -> s{_dcvetTemplateName = a})

instance AWSRequest
           DeleteCustomVerificationEmailTemplate
         where
        type Rs DeleteCustomVerificationEmailTemplate =
             DeleteCustomVerificationEmailTemplateResponse
        request = postQuery ses
        response
          = receiveNull
              DeleteCustomVerificationEmailTemplateResponse'

instance Hashable
           DeleteCustomVerificationEmailTemplate
         where

instance NFData DeleteCustomVerificationEmailTemplate
         where

instance ToHeaders
           DeleteCustomVerificationEmailTemplate
         where
        toHeaders = const mempty

instance ToPath DeleteCustomVerificationEmailTemplate
         where
        toPath = const "/"

instance ToQuery
           DeleteCustomVerificationEmailTemplate
         where
        toQuery DeleteCustomVerificationEmailTemplate'{..}
          = mconcat
              ["Action" =:
                 ("DeleteCustomVerificationEmailTemplate" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TemplateName" =: _dcvetTemplateName]

-- | /See:/ 'deleteCustomVerificationEmailTemplateResponse' smart constructor.
data DeleteCustomVerificationEmailTemplateResponse =
  DeleteCustomVerificationEmailTemplateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCustomVerificationEmailTemplateResponse' with the minimum fields required to make a request.
--
deleteCustomVerificationEmailTemplateResponse
    :: DeleteCustomVerificationEmailTemplateResponse
deleteCustomVerificationEmailTemplateResponse =
  DeleteCustomVerificationEmailTemplateResponse'


instance NFData
           DeleteCustomVerificationEmailTemplateResponse
         where
