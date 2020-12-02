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
-- Module      : Network.AWS.Glue.ListTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all trigger resources in this AWS account, or the resources with the specified tag. This operation allows you to see which resources are available in your account, and their names.
--
--
-- This operation takes the optional @Tags@ field, which you can use as a filter on the response so that tagged resources can be retrieved as a group. If you choose to use tags filtering, only resources with the tag are retrieved.
module Network.AWS.Glue.ListTriggers
  ( -- * Creating a Request
    listTriggers,
    ListTriggers,

    -- * Request Lenses
    ltNextToken,
    ltMaxResults,
    ltTags,
    ltDependentJobName,

    -- * Destructuring the Response
    listTriggersResponse,
    ListTriggersResponse,

    -- * Response Lenses
    ltrsTriggerNames,
    ltrsNextToken,
    ltrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTriggers' smart constructor.
data ListTriggers = ListTriggers'
  { _ltNextToken :: !(Maybe Text),
    _ltMaxResults :: !(Maybe Nat),
    _ltTags :: !(Maybe (Map Text (Text))),
    _ltDependentJobName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTriggers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken' - A continuation token, if this is a continuation request.
--
-- * 'ltMaxResults' - The maximum size of a list to return.
--
-- * 'ltTags' - Specifies to return only these tagged resources.
--
-- * 'ltDependentJobName' - The name of the job for which to retrieve triggers. The trigger that can start this job is returned. If there is no such trigger, all triggers are returned.
listTriggers ::
  ListTriggers
listTriggers =
  ListTriggers'
    { _ltNextToken = Nothing,
      _ltMaxResults = Nothing,
      _ltTags = Nothing,
      _ltDependentJobName = Nothing
    }

-- | A continuation token, if this is a continuation request.
ltNextToken :: Lens' ListTriggers (Maybe Text)
ltNextToken = lens _ltNextToken (\s a -> s {_ltNextToken = a})

-- | The maximum size of a list to return.
ltMaxResults :: Lens' ListTriggers (Maybe Natural)
ltMaxResults = lens _ltMaxResults (\s a -> s {_ltMaxResults = a}) . mapping _Nat

-- | Specifies to return only these tagged resources.
ltTags :: Lens' ListTriggers (HashMap Text (Text))
ltTags = lens _ltTags (\s a -> s {_ltTags = a}) . _Default . _Map

-- | The name of the job for which to retrieve triggers. The trigger that can start this job is returned. If there is no such trigger, all triggers are returned.
ltDependentJobName :: Lens' ListTriggers (Maybe Text)
ltDependentJobName = lens _ltDependentJobName (\s a -> s {_ltDependentJobName = a})

instance AWSRequest ListTriggers where
  type Rs ListTriggers = ListTriggersResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          ListTriggersResponse'
            <$> (x .?> "TriggerNames" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListTriggers

instance NFData ListTriggers

instance ToHeaders ListTriggers where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.ListTriggers" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListTriggers where
  toJSON ListTriggers' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _ltNextToken,
            ("MaxResults" .=) <$> _ltMaxResults,
            ("Tags" .=) <$> _ltTags,
            ("DependentJobName" .=) <$> _ltDependentJobName
          ]
      )

instance ToPath ListTriggers where
  toPath = const "/"

instance ToQuery ListTriggers where
  toQuery = const mempty

-- | /See:/ 'listTriggersResponse' smart constructor.
data ListTriggersResponse = ListTriggersResponse'
  { _ltrsTriggerNames ::
      !(Maybe [Text]),
    _ltrsNextToken :: !(Maybe Text),
    _ltrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTriggersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsTriggerNames' - The names of all triggers in the account, or the triggers with the specified tags.
--
-- * 'ltrsNextToken' - A continuation token, if the returned list does not contain the last metric available.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTriggersResponse ::
  -- | 'ltrsResponseStatus'
  Int ->
  ListTriggersResponse
listTriggersResponse pResponseStatus_ =
  ListTriggersResponse'
    { _ltrsTriggerNames = Nothing,
      _ltrsNextToken = Nothing,
      _ltrsResponseStatus = pResponseStatus_
    }

-- | The names of all triggers in the account, or the triggers with the specified tags.
ltrsTriggerNames :: Lens' ListTriggersResponse [Text]
ltrsTriggerNames = lens _ltrsTriggerNames (\s a -> s {_ltrsTriggerNames = a}) . _Default . _Coerce

-- | A continuation token, if the returned list does not contain the last metric available.
ltrsNextToken :: Lens' ListTriggersResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\s a -> s {_ltrsNextToken = a})

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTriggersResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\s a -> s {_ltrsResponseStatus = a})

instance NFData ListTriggersResponse
