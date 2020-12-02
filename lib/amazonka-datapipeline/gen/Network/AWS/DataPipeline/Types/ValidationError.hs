{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ValidationError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ValidationError where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines a validation error. Validation errors prevent pipeline activation. The set of validation errors that can be returned are defined by AWS Data Pipeline.
--
--
--
-- /See:/ 'validationError' smart constructor.
data ValidationError = ValidationError'
  { _veId :: !(Maybe Text),
    _veErrors :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidationError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veId' - The identifier of the object that contains the validation error.
--
-- * 'veErrors' - A description of the validation error.
validationError ::
  ValidationError
validationError =
  ValidationError' {_veId = Nothing, _veErrors = Nothing}

-- | The identifier of the object that contains the validation error.
veId :: Lens' ValidationError (Maybe Text)
veId = lens _veId (\s a -> s {_veId = a})

-- | A description of the validation error.
veErrors :: Lens' ValidationError [Text]
veErrors = lens _veErrors (\s a -> s {_veErrors = a}) . _Default . _Coerce

instance FromJSON ValidationError where
  parseJSON =
    withObject
      "ValidationError"
      ( \x ->
          ValidationError' <$> (x .:? "id") <*> (x .:? "errors" .!= mempty)
      )

instance Hashable ValidationError

instance NFData ValidationError
