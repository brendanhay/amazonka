{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResponseError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResponseError where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchTemplateErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the error that's returned when you cannot delete a launch template version.
--
--
--
-- /See:/ 'responseError' smart constructor.
data ResponseError = ResponseError'
  { _reCode ::
      !(Maybe LaunchTemplateErrorCode),
    _reMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResponseError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reCode' - The error code.
--
-- * 'reMessage' - The error message, if applicable.
responseError ::
  ResponseError
responseError =
  ResponseError' {_reCode = Nothing, _reMessage = Nothing}

-- | The error code.
reCode :: Lens' ResponseError (Maybe LaunchTemplateErrorCode)
reCode = lens _reCode (\s a -> s {_reCode = a})

-- | The error message, if applicable.
reMessage :: Lens' ResponseError (Maybe Text)
reMessage = lens _reMessage (\s a -> s {_reMessage = a})

instance FromXML ResponseError where
  parseXML x =
    ResponseError' <$> (x .@? "code") <*> (x .@? "message")

instance Hashable ResponseError

instance NFData ResponseError
