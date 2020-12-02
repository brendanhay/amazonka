{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Position
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Position where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the row and column of a location of a @Statement@ element in a policy document.
--
--
-- This data type is used as a member of the @'Statement' @ type.
--
--
-- /See:/ 'position' smart constructor.
data Position = Position'
  { _pLine :: !(Maybe Int),
    _pColumn :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Position' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pLine' - The line containing the specified position in the document.
--
-- * 'pColumn' - The column in the line containing the specified position in the document.
position ::
  Position
position = Position' {_pLine = Nothing, _pColumn = Nothing}

-- | The line containing the specified position in the document.
pLine :: Lens' Position (Maybe Int)
pLine = lens _pLine (\s a -> s {_pLine = a})

-- | The column in the line containing the specified position in the document.
pColumn :: Lens' Position (Maybe Int)
pColumn = lens _pColumn (\s a -> s {_pColumn = a})

instance FromXML Position where
  parseXML x = Position' <$> (x .@? "Line") <*> (x .@? "Column")

instance Hashable Position

instance NFData Position
