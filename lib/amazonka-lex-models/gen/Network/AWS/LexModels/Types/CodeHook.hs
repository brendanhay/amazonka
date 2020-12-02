{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.CodeHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.CodeHook where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a Lambda function that verifies requests to a bot or fulfills the user's request to a bot..
--
--
--
-- /See:/ 'codeHook' smart constructor.
data CodeHook = CodeHook'
  { _chUri :: !Text,
    _chMessageVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chUri' - The Amazon Resource Name (ARN) of the Lambda function.
--
-- * 'chMessageVersion' - The version of the request-response that you want Amazon Lex to use to invoke your Lambda function. For more information, see 'using-lambda' .
codeHook ::
  -- | 'chUri'
  Text ->
  -- | 'chMessageVersion'
  Text ->
  CodeHook
codeHook pUri_ pMessageVersion_ =
  CodeHook' {_chUri = pUri_, _chMessageVersion = pMessageVersion_}

-- | The Amazon Resource Name (ARN) of the Lambda function.
chUri :: Lens' CodeHook Text
chUri = lens _chUri (\s a -> s {_chUri = a})

-- | The version of the request-response that you want Amazon Lex to use to invoke your Lambda function. For more information, see 'using-lambda' .
chMessageVersion :: Lens' CodeHook Text
chMessageVersion = lens _chMessageVersion (\s a -> s {_chMessageVersion = a})

instance FromJSON CodeHook where
  parseJSON =
    withObject
      "CodeHook"
      (\x -> CodeHook' <$> (x .: "uri") <*> (x .: "messageVersion"))

instance Hashable CodeHook

instance NFData CodeHook

instance ToJSON CodeHook where
  toJSON CodeHook' {..} =
    object
      ( catMaybes
          [ Just ("uri" .= _chUri),
            Just ("messageVersion" .= _chMessageVersion)
          ]
      )
