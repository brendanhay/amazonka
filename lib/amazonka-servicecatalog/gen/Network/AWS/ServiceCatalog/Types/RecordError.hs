{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RecordError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordError where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The error code and description resulting from an operation.
--
--
--
-- /See:/ 'recordError' smart constructor.
data RecordError = RecordError'
  { _reCode :: !(Maybe Text),
    _reDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecordError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reCode' - The numeric value of the error.
--
-- * 'reDescription' - The description of the error.
recordError ::
  RecordError
recordError =
  RecordError' {_reCode = Nothing, _reDescription = Nothing}

-- | The numeric value of the error.
reCode :: Lens' RecordError (Maybe Text)
reCode = lens _reCode (\s a -> s {_reCode = a})

-- | The description of the error.
reDescription :: Lens' RecordError (Maybe Text)
reDescription = lens _reDescription (\s a -> s {_reDescription = a})

instance FromJSON RecordError where
  parseJSON =
    withObject
      "RecordError"
      (\x -> RecordError' <$> (x .:? "Code") <*> (x .:? "Description"))

instance Hashable RecordError

instance NFData RecordError
