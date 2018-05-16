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
-- Module      : Network.AWS.SES.ListTemplates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the email templates present in your Amazon SES account.
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.ListTemplates
    (
    -- * Creating a Request
      listTemplates
    , ListTemplates
    -- * Request Lenses
    , ltNextToken
    , ltMaxItems

    -- * Destructuring the Response
    , listTemplatesResponse
    , ListTemplatesResponse
    -- * Response Lenses
    , ltrsTemplatesMetadata
    , ltrsNextToken
    , ltrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | /See:/ 'listTemplates' smart constructor.
data ListTemplates = ListTemplates'
  { _ltNextToken :: !(Maybe Text)
  , _ltMaxItems  :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTemplates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken' - A token returned from a previous call to @ListTemplates@ to indicate the position in the list of email templates.
--
-- * 'ltMaxItems' - The maximum number of templates to return. This value must be at least 1 and less than or equal to 10. If you do not specify a value, or if you specify a value less than 1 or greater than 10, the operation will return up to 10 results.
listTemplates
    :: ListTemplates
listTemplates = ListTemplates' {_ltNextToken = Nothing, _ltMaxItems = Nothing}


-- | A token returned from a previous call to @ListTemplates@ to indicate the position in the list of email templates.
ltNextToken :: Lens' ListTemplates (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | The maximum number of templates to return. This value must be at least 1 and less than or equal to 10. If you do not specify a value, or if you specify a value less than 1 or greater than 10, the operation will return up to 10 results.
ltMaxItems :: Lens' ListTemplates (Maybe Int)
ltMaxItems = lens _ltMaxItems (\ s a -> s{_ltMaxItems = a})

instance AWSRequest ListTemplates where
        type Rs ListTemplates = ListTemplatesResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "ListTemplatesResult"
              (\ s h x ->
                 ListTemplatesResponse' <$>
                   (x .@? "TemplatesMetadata" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListTemplates where

instance NFData ListTemplates where

instance ToHeaders ListTemplates where
        toHeaders = const mempty

instance ToPath ListTemplates where
        toPath = const "/"

instance ToQuery ListTemplates where
        toQuery ListTemplates'{..}
          = mconcat
              ["Action" =: ("ListTemplates" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "NextToken" =: _ltNextToken,
               "MaxItems" =: _ltMaxItems]

-- | /See:/ 'listTemplatesResponse' smart constructor.
data ListTemplatesResponse = ListTemplatesResponse'
  { _ltrsTemplatesMetadata :: !(Maybe [TemplateMetadata])
  , _ltrsNextToken         :: !(Maybe Text)
  , _ltrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTemplatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsTemplatesMetadata' - An array the contains the name and creation time stamp for each template in your Amazon SES account.
--
-- * 'ltrsNextToken' - A token indicating that there are additional email templates available to be listed. Pass this token to a subsequent call to @ListTemplates@ to retrieve the next 50 email templates.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTemplatesResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTemplatesResponse
listTemplatesResponse pResponseStatus_ =
  ListTemplatesResponse'
    { _ltrsTemplatesMetadata = Nothing
    , _ltrsNextToken = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | An array the contains the name and creation time stamp for each template in your Amazon SES account.
ltrsTemplatesMetadata :: Lens' ListTemplatesResponse [TemplateMetadata]
ltrsTemplatesMetadata = lens _ltrsTemplatesMetadata (\ s a -> s{_ltrsTemplatesMetadata = a}) . _Default . _Coerce

-- | A token indicating that there are additional email templates available to be listed. Pass this token to a subsequent call to @ListTemplates@ to retrieve the next 50 email templates.
ltrsNextToken :: Lens' ListTemplatesResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTemplatesResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTemplatesResponse where
