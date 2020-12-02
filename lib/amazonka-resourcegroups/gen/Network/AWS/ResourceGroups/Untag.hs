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
-- Module      : Network.AWS.ResourceGroups.Untag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes tags from a specified resource group.
module Network.AWS.ResourceGroups.Untag
  ( -- * Creating a Request
    untag,
    Untag,

    -- * Request Lenses
    uARN,
    uKeys,

    -- * Destructuring the Response
    untagResponse,
    UntagResponse,

    -- * Response Lenses
    ursARN,
    ursKeys,
    ursResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'untag' smart constructor.
data Untag = Untag' {_uARN :: !Text, _uKeys :: ![Text]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Untag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uARN' - The ARN of the resource group from which to remove tags. The command removed both the specified keys and any values associated with those keys.
--
-- * 'uKeys' - The keys of the tags to be removed.
untag ::
  -- | 'uARN'
  Text ->
  Untag
untag pARN_ = Untag' {_uARN = pARN_, _uKeys = mempty}

-- | The ARN of the resource group from which to remove tags. The command removed both the specified keys and any values associated with those keys.
uARN :: Lens' Untag Text
uARN = lens _uARN (\s a -> s {_uARN = a})

-- | The keys of the tags to be removed.
uKeys :: Lens' Untag [Text]
uKeys = lens _uKeys (\s a -> s {_uKeys = a}) . _Coerce

instance AWSRequest Untag where
  type Rs Untag = UntagResponse
  request = patchJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          UntagResponse'
            <$> (x .?> "Arn")
            <*> (x .?> "Keys" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable Untag

instance NFData Untag

instance ToHeaders Untag where
  toHeaders = const mempty

instance ToJSON Untag where
  toJSON Untag' {..} = object (catMaybes [Just ("Keys" .= _uKeys)])

instance ToPath Untag where
  toPath Untag' {..} = mconcat ["/resources/", toBS _uARN, "/tags"]

instance ToQuery Untag where
  toQuery = const mempty

-- | /See:/ 'untagResponse' smart constructor.
data UntagResponse = UntagResponse'
  { _ursARN :: !(Maybe Text),
    _ursKeys :: !(Maybe [Text]),
    _ursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursARN' - The ARN of the resource group from which tags have been removed.
--
-- * 'ursKeys' - The keys of the tags that were removed.
--
-- * 'ursResponseStatus' - -- | The response status code.
untagResponse ::
  -- | 'ursResponseStatus'
  Int ->
  UntagResponse
untagResponse pResponseStatus_ =
  UntagResponse'
    { _ursARN = Nothing,
      _ursKeys = Nothing,
      _ursResponseStatus = pResponseStatus_
    }

-- | The ARN of the resource group from which tags have been removed.
ursARN :: Lens' UntagResponse (Maybe Text)
ursARN = lens _ursARN (\s a -> s {_ursARN = a})

-- | The keys of the tags that were removed.
ursKeys :: Lens' UntagResponse [Text]
ursKeys = lens _ursKeys (\s a -> s {_ursKeys = a}) . _Default . _Coerce

-- | -- | The response status code.
ursResponseStatus :: Lens' UntagResponse Int
ursResponseStatus = lens _ursResponseStatus (\s a -> s {_ursResponseStatus = a})

instance NFData UntagResponse
