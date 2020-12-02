{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CodeGenNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CodeGenNode where

import Network.AWS.Glue.Types.CodeGenNodeArg
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a node in a directed acyclic graph (DAG)
--
--
--
-- /See:/ 'codeGenNode' smart constructor.
data CodeGenNode = CodeGenNode'
  { _cgnLineNumber :: !(Maybe Int),
    _cgnId :: !Text,
    _cgnNodeType :: !Text,
    _cgnArgs :: ![CodeGenNodeArg]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeGenNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgnLineNumber' - The line number of the node.
--
-- * 'cgnId' - A node identifier that is unique within the node's graph.
--
-- * 'cgnNodeType' - The type of node that this is.
--
-- * 'cgnArgs' - Properties of the node, in the form of name-value pairs.
codeGenNode ::
  -- | 'cgnId'
  Text ->
  -- | 'cgnNodeType'
  Text ->
  CodeGenNode
codeGenNode pId_ pNodeType_ =
  CodeGenNode'
    { _cgnLineNumber = Nothing,
      _cgnId = pId_,
      _cgnNodeType = pNodeType_,
      _cgnArgs = mempty
    }

-- | The line number of the node.
cgnLineNumber :: Lens' CodeGenNode (Maybe Int)
cgnLineNumber = lens _cgnLineNumber (\s a -> s {_cgnLineNumber = a})

-- | A node identifier that is unique within the node's graph.
cgnId :: Lens' CodeGenNode Text
cgnId = lens _cgnId (\s a -> s {_cgnId = a})

-- | The type of node that this is.
cgnNodeType :: Lens' CodeGenNode Text
cgnNodeType = lens _cgnNodeType (\s a -> s {_cgnNodeType = a})

-- | Properties of the node, in the form of name-value pairs.
cgnArgs :: Lens' CodeGenNode [CodeGenNodeArg]
cgnArgs = lens _cgnArgs (\s a -> s {_cgnArgs = a}) . _Coerce

instance FromJSON CodeGenNode where
  parseJSON =
    withObject
      "CodeGenNode"
      ( \x ->
          CodeGenNode'
            <$> (x .:? "LineNumber")
            <*> (x .: "Id")
            <*> (x .: "NodeType")
            <*> (x .:? "Args" .!= mempty)
      )

instance Hashable CodeGenNode

instance NFData CodeGenNode

instance ToJSON CodeGenNode where
  toJSON CodeGenNode' {..} =
    object
      ( catMaybes
          [ ("LineNumber" .=) <$> _cgnLineNumber,
            Just ("Id" .= _cgnId),
            Just ("NodeType" .= _cgnNodeType),
            Just ("Args" .= _cgnArgs)
          ]
      )
