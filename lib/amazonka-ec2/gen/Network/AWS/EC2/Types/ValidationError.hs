{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ValidationError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ValidationError where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The error code and error message that is returned for a parameter or parameter combination that is not valid when a new launch template or new version of a launch template is created.
--
--
--
-- /See:/ 'validationError' smart constructor.
data ValidationError = ValidationError'
  { _veCode :: !(Maybe Text),
    _veMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidationError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veCode' - The error code that indicates why the parameter or parameter combination is not valid. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- * 'veMessage' - The error message that describes why the parameter or parameter combination is not valid. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
validationError ::
  ValidationError
validationError =
  ValidationError' {_veCode = Nothing, _veMessage = Nothing}

-- | The error code that indicates why the parameter or parameter combination is not valid. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
veCode :: Lens' ValidationError (Maybe Text)
veCode = lens _veCode (\s a -> s {_veCode = a})

-- | The error message that describes why the parameter or parameter combination is not valid. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
veMessage :: Lens' ValidationError (Maybe Text)
veMessage = lens _veMessage (\s a -> s {_veMessage = a})

instance FromXML ValidationError where
  parseXML x =
    ValidationError' <$> (x .@? "code") <*> (x .@? "message")

instance Hashable ValidationError

instance NFData ValidationError
