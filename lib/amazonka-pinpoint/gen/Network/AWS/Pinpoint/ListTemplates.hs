{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.ListTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the message templates that are associated with your Amazon Pinpoint account.
module Network.AWS.Pinpoint.ListTemplates
  ( -- * Creating a Request
    listTemplates,
    ListTemplates,

    -- * Request Lenses
    ltTemplateType,
    ltPrefix,
    ltNextToken,
    ltPageSize,

    -- * Destructuring the Response
    listTemplatesResponse,
    ListTemplatesResponse,

    -- * Response Lenses
    ltrsResponseStatus,
    ltrsTemplatesResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTemplates' smart constructor.
data ListTemplates = ListTemplates'
  { _ltTemplateType ::
      !(Maybe Text),
    _ltPrefix :: !(Maybe Text),
    _ltNextToken :: !(Maybe Text),
    _ltPageSize :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTemplates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltTemplateType' - The type of message template to include in the results. Valid values are: EMAIL, PUSH, SMS, and VOICE. To include all types of templates in the results, don't include this parameter in your request.
--
-- * 'ltPrefix' - The substring to match in the names of the message templates to include in the results. If you specify this value, Amazon Pinpoint returns only those templates whose names begin with the value that you specify.
--
-- * 'ltNextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'ltPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
listTemplates ::
  ListTemplates
listTemplates =
  ListTemplates'
    { _ltTemplateType = Nothing,
      _ltPrefix = Nothing,
      _ltNextToken = Nothing,
      _ltPageSize = Nothing
    }

-- | The type of message template to include in the results. Valid values are: EMAIL, PUSH, SMS, and VOICE. To include all types of templates in the results, don't include this parameter in your request.
ltTemplateType :: Lens' ListTemplates (Maybe Text)
ltTemplateType = lens _ltTemplateType (\s a -> s {_ltTemplateType = a})

-- | The substring to match in the names of the message templates to include in the results. If you specify this value, Amazon Pinpoint returns only those templates whose names begin with the value that you specify.
ltPrefix :: Lens' ListTemplates (Maybe Text)
ltPrefix = lens _ltPrefix (\s a -> s {_ltPrefix = a})

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
ltNextToken :: Lens' ListTemplates (Maybe Text)
ltNextToken = lens _ltNextToken (\s a -> s {_ltNextToken = a})

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
ltPageSize :: Lens' ListTemplates (Maybe Text)
ltPageSize = lens _ltPageSize (\s a -> s {_ltPageSize = a})

instance AWSRequest ListTemplates where
  type Rs ListTemplates = ListTemplatesResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          ListTemplatesResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable ListTemplates

instance NFData ListTemplates

instance ToHeaders ListTemplates where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListTemplates where
  toPath = const "/v1/templates"

instance ToQuery ListTemplates where
  toQuery ListTemplates' {..} =
    mconcat
      [ "template-type" =: _ltTemplateType,
        "prefix" =: _ltPrefix,
        "next-token" =: _ltNextToken,
        "page-size" =: _ltPageSize
      ]

-- | /See:/ 'listTemplatesResponse' smart constructor.
data ListTemplatesResponse = ListTemplatesResponse'
  { _ltrsResponseStatus ::
      !Int,
    _ltrsTemplatesResponse :: !TemplatesResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTemplatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsResponseStatus' - -- | The response status code.
--
-- * 'ltrsTemplatesResponse' - Undocumented member.
listTemplatesResponse ::
  -- | 'ltrsResponseStatus'
  Int ->
  -- | 'ltrsTemplatesResponse'
  TemplatesResponse ->
  ListTemplatesResponse
listTemplatesResponse pResponseStatus_ pTemplatesResponse_ =
  ListTemplatesResponse'
    { _ltrsResponseStatus = pResponseStatus_,
      _ltrsTemplatesResponse = pTemplatesResponse_
    }

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTemplatesResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\s a -> s {_ltrsResponseStatus = a})

-- | Undocumented member.
ltrsTemplatesResponse :: Lens' ListTemplatesResponse TemplatesResponse
ltrsTemplatesResponse = lens _ltrsTemplatesResponse (\s a -> s {_ltrsTemplatesResponse = a})

instance NFData ListTemplatesResponse
