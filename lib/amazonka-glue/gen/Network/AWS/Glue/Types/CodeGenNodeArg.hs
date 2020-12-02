{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CodeGenNodeArg
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CodeGenNodeArg where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An argument or property of a node.
--
--
--
-- /See:/ 'codeGenNodeArg' smart constructor.
data CodeGenNodeArg = CodeGenNodeArg'
  { _cgnaParam :: !(Maybe Bool),
    _cgnaName :: !Text,
    _cgnaValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeGenNodeArg' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgnaParam' - True if the value is used as a parameter.
--
-- * 'cgnaName' - The name of the argument or property.
--
-- * 'cgnaValue' - The value of the argument or property.
codeGenNodeArg ::
  -- | 'cgnaName'
  Text ->
  -- | 'cgnaValue'
  Text ->
  CodeGenNodeArg
codeGenNodeArg pName_ pValue_ =
  CodeGenNodeArg'
    { _cgnaParam = Nothing,
      _cgnaName = pName_,
      _cgnaValue = pValue_
    }

-- | True if the value is used as a parameter.
cgnaParam :: Lens' CodeGenNodeArg (Maybe Bool)
cgnaParam = lens _cgnaParam (\s a -> s {_cgnaParam = a})

-- | The name of the argument or property.
cgnaName :: Lens' CodeGenNodeArg Text
cgnaName = lens _cgnaName (\s a -> s {_cgnaName = a})

-- | The value of the argument or property.
cgnaValue :: Lens' CodeGenNodeArg Text
cgnaValue = lens _cgnaValue (\s a -> s {_cgnaValue = a})

instance FromJSON CodeGenNodeArg where
  parseJSON =
    withObject
      "CodeGenNodeArg"
      ( \x ->
          CodeGenNodeArg'
            <$> (x .:? "Param") <*> (x .: "Name") <*> (x .: "Value")
      )

instance Hashable CodeGenNodeArg

instance NFData CodeGenNodeArg

instance ToJSON CodeGenNodeArg where
  toJSON CodeGenNodeArg' {..} =
    object
      ( catMaybes
          [ ("Param" .=) <$> _cgnaParam,
            Just ("Name" .= _cgnaName),
            Just ("Value" .= _cgnaValue)
          ]
      )
