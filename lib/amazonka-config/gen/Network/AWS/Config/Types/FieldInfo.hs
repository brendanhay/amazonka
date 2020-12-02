{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.FieldInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FieldInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the fields such as name of the field.
--
--
--
-- /See:/ 'fieldInfo' smart constructor.
newtype FieldInfo = FieldInfo' {_fiName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FieldInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fiName' - Name of the field.
fieldInfo ::
  FieldInfo
fieldInfo = FieldInfo' {_fiName = Nothing}

-- | Name of the field.
fiName :: Lens' FieldInfo (Maybe Text)
fiName = lens _fiName (\s a -> s {_fiName = a})

instance FromJSON FieldInfo where
  parseJSON =
    withObject "FieldInfo" (\x -> FieldInfo' <$> (x .:? "Name"))

instance Hashable FieldInfo

instance NFData FieldInfo
