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
-- Module      : Network.AWS.SES.ListCustomVerificationEmailTemplates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing custom verification email templates for your account.
--
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
--
-- You can execute this operation no more than once per second.
--
--
-- This operation returns paginated results.
module Network.AWS.SES.ListCustomVerificationEmailTemplates
    (
    -- * Creating a Request
      listCustomVerificationEmailTemplates
    , ListCustomVerificationEmailTemplates
    -- * Request Lenses
    , lcvetNextToken
    , lcvetMaxResults

    -- * Destructuring the Response
    , listCustomVerificationEmailTemplatesResponse
    , ListCustomVerificationEmailTemplatesResponse
    -- * Response Lenses
    , lcvetrsNextToken
    , lcvetrsCustomVerificationEmailTemplates
    , lcvetrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to list the existing custom verification email templates for your account.
--
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
--
--
-- /See:/ 'listCustomVerificationEmailTemplates' smart constructor.
data ListCustomVerificationEmailTemplates = ListCustomVerificationEmailTemplates'
  { _lcvetNextToken  :: !(Maybe Text)
  , _lcvetMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCustomVerificationEmailTemplates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcvetNextToken' - An array the contains the name and creation time stamp for each template in your Amazon SES account.
--
-- * 'lcvetMaxResults' - The maximum number of custom verification email templates to return. This value must be at least 1 and less than or equal to 50. If you do not specify a value, or if you specify a value less than 1 or greater than 50, the operation will return up to 50 results.
listCustomVerificationEmailTemplates
    :: ListCustomVerificationEmailTemplates
listCustomVerificationEmailTemplates =
  ListCustomVerificationEmailTemplates'
    {_lcvetNextToken = Nothing, _lcvetMaxResults = Nothing}


-- | An array the contains the name and creation time stamp for each template in your Amazon SES account.
lcvetNextToken :: Lens' ListCustomVerificationEmailTemplates (Maybe Text)
lcvetNextToken = lens _lcvetNextToken (\ s a -> s{_lcvetNextToken = a})

-- | The maximum number of custom verification email templates to return. This value must be at least 1 and less than or equal to 50. If you do not specify a value, or if you specify a value less than 1 or greater than 50, the operation will return up to 50 results.
lcvetMaxResults :: Lens' ListCustomVerificationEmailTemplates (Maybe Natural)
lcvetMaxResults = lens _lcvetMaxResults (\ s a -> s{_lcvetMaxResults = a}) . mapping _Nat

instance AWSPager
           ListCustomVerificationEmailTemplates
         where
        page rq rs
          | stop (rs ^. lcvetrsNextToken) = Nothing
          | stop
              (rs ^. lcvetrsCustomVerificationEmailTemplates)
            = Nothing
          | otherwise =
            Just $ rq & lcvetNextToken .~ rs ^. lcvetrsNextToken

instance AWSRequest
           ListCustomVerificationEmailTemplates
         where
        type Rs ListCustomVerificationEmailTemplates =
             ListCustomVerificationEmailTemplatesResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "ListCustomVerificationEmailTemplatesResult"
              (\ s h x ->
                 ListCustomVerificationEmailTemplatesResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "CustomVerificationEmailTemplates" .!@ mempty
                        >>= may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable
           ListCustomVerificationEmailTemplates
         where

instance NFData ListCustomVerificationEmailTemplates
         where

instance ToHeaders
           ListCustomVerificationEmailTemplates
         where
        toHeaders = const mempty

instance ToPath ListCustomVerificationEmailTemplates
         where
        toPath = const "/"

instance ToQuery ListCustomVerificationEmailTemplates
         where
        toQuery ListCustomVerificationEmailTemplates'{..}
          = mconcat
              ["Action" =:
                 ("ListCustomVerificationEmailTemplates" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "NextToken" =: _lcvetNextToken,
               "MaxResults" =: _lcvetMaxResults]

-- | A paginated list of custom verification email templates.
--
--
--
-- /See:/ 'listCustomVerificationEmailTemplatesResponse' smart constructor.
data ListCustomVerificationEmailTemplatesResponse = ListCustomVerificationEmailTemplatesResponse'
  { _lcvetrsNextToken :: !(Maybe Text)
  , _lcvetrsCustomVerificationEmailTemplates :: !(Maybe [CustomVerificationEmailTemplate])
  , _lcvetrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCustomVerificationEmailTemplatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcvetrsNextToken' - A token indicating that there are additional custom verification email templates available to be listed. Pass this token to a subsequent call to @ListTemplates@ to retrieve the next 50 custom verification email templates.
--
-- * 'lcvetrsCustomVerificationEmailTemplates' - A list of the custom verification email templates that exist in your account.
--
-- * 'lcvetrsResponseStatus' - -- | The response status code.
listCustomVerificationEmailTemplatesResponse
    :: Int -- ^ 'lcvetrsResponseStatus'
    -> ListCustomVerificationEmailTemplatesResponse
listCustomVerificationEmailTemplatesResponse pResponseStatus_ =
  ListCustomVerificationEmailTemplatesResponse'
    { _lcvetrsNextToken = Nothing
    , _lcvetrsCustomVerificationEmailTemplates = Nothing
    , _lcvetrsResponseStatus = pResponseStatus_
    }


-- | A token indicating that there are additional custom verification email templates available to be listed. Pass this token to a subsequent call to @ListTemplates@ to retrieve the next 50 custom verification email templates.
lcvetrsNextToken :: Lens' ListCustomVerificationEmailTemplatesResponse (Maybe Text)
lcvetrsNextToken = lens _lcvetrsNextToken (\ s a -> s{_lcvetrsNextToken = a})

-- | A list of the custom verification email templates that exist in your account.
lcvetrsCustomVerificationEmailTemplates :: Lens' ListCustomVerificationEmailTemplatesResponse [CustomVerificationEmailTemplate]
lcvetrsCustomVerificationEmailTemplates = lens _lcvetrsCustomVerificationEmailTemplates (\ s a -> s{_lcvetrsCustomVerificationEmailTemplates = a}) . _Default . _Coerce

-- | -- | The response status code.
lcvetrsResponseStatus :: Lens' ListCustomVerificationEmailTemplatesResponse Int
lcvetrsResponseStatus = lens _lcvetrsResponseStatus (\ s a -> s{_lcvetrsResponseStatus = a})

instance NFData
           ListCustomVerificationEmailTemplatesResponse
         where
