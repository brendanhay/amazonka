{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ValidationWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ValidationWarning where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines a validation warning. Validation warnings do not prevent pipeline activation. The set of validation warnings that can be returned are defined by AWS Data Pipeline.
--
--
--
-- /See:/ 'validationWarning' smart constructor.
data ValidationWarning = ValidationWarning'
  { _vwWarnings ::
      !(Maybe [Text]),
    _vwId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidationWarning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vwWarnings' - A description of the validation warning.
--
-- * 'vwId' - The identifier of the object that contains the validation warning.
validationWarning ::
  ValidationWarning
validationWarning =
  ValidationWarning' {_vwWarnings = Nothing, _vwId = Nothing}

-- | A description of the validation warning.
vwWarnings :: Lens' ValidationWarning [Text]
vwWarnings = lens _vwWarnings (\s a -> s {_vwWarnings = a}) . _Default . _Coerce

-- | The identifier of the object that contains the validation warning.
vwId :: Lens' ValidationWarning (Maybe Text)
vwId = lens _vwId (\s a -> s {_vwId = a})

instance FromJSON ValidationWarning where
  parseJSON =
    withObject
      "ValidationWarning"
      ( \x ->
          ValidationWarning'
            <$> (x .:? "warnings" .!= mempty) <*> (x .:? "id")
      )

instance Hashable ValidationWarning

instance NFData ValidationWarning
