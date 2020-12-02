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
-- Module      : Network.AWS.GameLift.DescribeScript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for a Realtime script.
--
--
-- To request a script record, specify the script ID. If successful, an object containing the script properties is returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
--
-- __Related operations__
--
--     * 'CreateScript'
--
--     * 'ListScripts'
--
--     * 'DescribeScript'
--
--     * 'UpdateScript'
--
--     * 'DeleteScript'
module Network.AWS.GameLift.DescribeScript
  ( -- * Creating a Request
    describeScript,
    DescribeScript,

    -- * Request Lenses
    dScriptId,

    -- * Destructuring the Response
    describeScriptResponse,
    DescribeScriptResponse,

    -- * Response Lenses
    dsrsScript,
    dsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScript' smart constructor.
newtype DescribeScript = DescribeScript' {_dScriptId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeScript' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dScriptId' - A unique identifier for a Realtime script to retrieve properties for. You can use either the script ID or ARN value.
describeScript ::
  -- | 'dScriptId'
  Text ->
  DescribeScript
describeScript pScriptId_ =
  DescribeScript' {_dScriptId = pScriptId_}

-- | A unique identifier for a Realtime script to retrieve properties for. You can use either the script ID or ARN value.
dScriptId :: Lens' DescribeScript Text
dScriptId = lens _dScriptId (\s a -> s {_dScriptId = a})

instance AWSRequest DescribeScript where
  type Rs DescribeScript = DescribeScriptResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          DescribeScriptResponse'
            <$> (x .?> "Script") <*> (pure (fromEnum s))
      )

instance Hashable DescribeScript

instance NFData DescribeScript

instance ToHeaders DescribeScript where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.DescribeScript" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeScript where
  toJSON DescribeScript' {..} =
    object (catMaybes [Just ("ScriptId" .= _dScriptId)])

instance ToPath DescribeScript where
  toPath = const "/"

instance ToQuery DescribeScript where
  toQuery = const mempty

-- | /See:/ 'describeScriptResponse' smart constructor.
data DescribeScriptResponse = DescribeScriptResponse'
  { _dsrsScript ::
      !(Maybe Script),
    _dsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeScriptResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsScript' - A set of properties describing the requested script.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
describeScriptResponse ::
  -- | 'dsrsResponseStatus'
  Int ->
  DescribeScriptResponse
describeScriptResponse pResponseStatus_ =
  DescribeScriptResponse'
    { _dsrsScript = Nothing,
      _dsrsResponseStatus = pResponseStatus_
    }

-- | A set of properties describing the requested script.
dsrsScript :: Lens' DescribeScriptResponse (Maybe Script)
dsrsScript = lens _dsrsScript (\s a -> s {_dsrsScript = a})

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DescribeScriptResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\s a -> s {_dsrsResponseStatus = a})

instance NFData DescribeScriptResponse
