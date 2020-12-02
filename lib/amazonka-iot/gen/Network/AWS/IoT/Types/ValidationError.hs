{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ValidationError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ValidationError where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an error found in a behavior specification.
--
--
--
-- /See:/ 'validationError' smart constructor.
newtype ValidationError = ValidationError'
  { _veErrorMessage ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidationError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veErrorMessage' - The description of an error found in the behaviors.
validationError ::
  ValidationError
validationError = ValidationError' {_veErrorMessage = Nothing}

-- | The description of an error found in the behaviors.
veErrorMessage :: Lens' ValidationError (Maybe Text)
veErrorMessage = lens _veErrorMessage (\s a -> s {_veErrorMessage = a})

instance FromJSON ValidationError where
  parseJSON =
    withObject
      "ValidationError"
      (\x -> ValidationError' <$> (x .:? "errorMessage"))

instance Hashable ValidationError

instance NFData ValidationError
