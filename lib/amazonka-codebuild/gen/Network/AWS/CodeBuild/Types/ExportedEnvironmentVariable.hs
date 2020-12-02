{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an exported environment variable.
--
--
--
-- /See:/ 'exportedEnvironmentVariable' smart constructor.
data ExportedEnvironmentVariable = ExportedEnvironmentVariable'
  { _eevValue ::
      !(Maybe Text),
    _eevName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportedEnvironmentVariable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eevValue' - The value assigned to this exported environment variable.
--
-- * 'eevName' - The name of this exported environment variable.
exportedEnvironmentVariable ::
  ExportedEnvironmentVariable
exportedEnvironmentVariable =
  ExportedEnvironmentVariable'
    { _eevValue = Nothing,
      _eevName = Nothing
    }

-- | The value assigned to this exported environment variable.
eevValue :: Lens' ExportedEnvironmentVariable (Maybe Text)
eevValue = lens _eevValue (\s a -> s {_eevValue = a})

-- | The name of this exported environment variable.
eevName :: Lens' ExportedEnvironmentVariable (Maybe Text)
eevName = lens _eevName (\s a -> s {_eevName = a})

instance FromJSON ExportedEnvironmentVariable where
  parseJSON =
    withObject
      "ExportedEnvironmentVariable"
      ( \x ->
          ExportedEnvironmentVariable'
            <$> (x .:? "value") <*> (x .:? "name")
      )

instance Hashable ExportedEnvironmentVariable

instance NFData ExportedEnvironmentVariable
