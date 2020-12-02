{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CodeGenEdge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CodeGenEdge where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a directional edge in a directed acyclic graph (DAG).
--
--
--
-- /See:/ 'codeGenEdge' smart constructor.
data CodeGenEdge = CodeGenEdge'
  { _cgeTargetParameter ::
      !(Maybe Text),
    _cgeSource :: !Text,
    _cgeTarget :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeGenEdge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgeTargetParameter' - The target of the edge.
--
-- * 'cgeSource' - The ID of the node at which the edge starts.
--
-- * 'cgeTarget' - The ID of the node at which the edge ends.
codeGenEdge ::
  -- | 'cgeSource'
  Text ->
  -- | 'cgeTarget'
  Text ->
  CodeGenEdge
codeGenEdge pSource_ pTarget_ =
  CodeGenEdge'
    { _cgeTargetParameter = Nothing,
      _cgeSource = pSource_,
      _cgeTarget = pTarget_
    }

-- | The target of the edge.
cgeTargetParameter :: Lens' CodeGenEdge (Maybe Text)
cgeTargetParameter = lens _cgeTargetParameter (\s a -> s {_cgeTargetParameter = a})

-- | The ID of the node at which the edge starts.
cgeSource :: Lens' CodeGenEdge Text
cgeSource = lens _cgeSource (\s a -> s {_cgeSource = a})

-- | The ID of the node at which the edge ends.
cgeTarget :: Lens' CodeGenEdge Text
cgeTarget = lens _cgeTarget (\s a -> s {_cgeTarget = a})

instance FromJSON CodeGenEdge where
  parseJSON =
    withObject
      "CodeGenEdge"
      ( \x ->
          CodeGenEdge'
            <$> (x .:? "TargetParameter") <*> (x .: "Source") <*> (x .: "Target")
      )

instance Hashable CodeGenEdge

instance NFData CodeGenEdge

instance ToJSON CodeGenEdge where
  toJSON CodeGenEdge' {..} =
    object
      ( catMaybes
          [ ("TargetParameter" .=) <$> _cgeTargetParameter,
            Just ("Source" .= _cgeSource),
            Just ("Target" .= _cgeTarget)
          ]
      )
