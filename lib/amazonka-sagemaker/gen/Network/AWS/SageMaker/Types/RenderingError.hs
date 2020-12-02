{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RenderingError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RenderingError where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A description of an error that occurred while rendering the template.
--
--
--
-- /See:/ 'renderingError' smart constructor.
data RenderingError = RenderingError'
  { _reCode :: !Text,
    _reMessage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RenderingError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reCode' - A unique identifier for a specific class of errors.
--
-- * 'reMessage' - A human-readable message describing the error.
renderingError ::
  -- | 'reCode'
  Text ->
  -- | 'reMessage'
  Text ->
  RenderingError
renderingError pCode_ pMessage_ =
  RenderingError' {_reCode = pCode_, _reMessage = pMessage_}

-- | A unique identifier for a specific class of errors.
reCode :: Lens' RenderingError Text
reCode = lens _reCode (\s a -> s {_reCode = a})

-- | A human-readable message describing the error.
reMessage :: Lens' RenderingError Text
reMessage = lens _reMessage (\s a -> s {_reMessage = a})

instance FromJSON RenderingError where
  parseJSON =
    withObject
      "RenderingError"
      (\x -> RenderingError' <$> (x .: "Code") <*> (x .: "Message"))

instance Hashable RenderingError

instance NFData RenderingError
