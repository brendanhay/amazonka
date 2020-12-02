{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.BlockerDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.BlockerDeclaration where

import Network.AWS.CodePipeline.Types.BlockerType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Reserved for future use.
--
--
--
-- /See:/ 'blockerDeclaration' smart constructor.
data BlockerDeclaration = BlockerDeclaration'
  { _bdName :: !Text,
    _bdType :: !BlockerType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BlockerDeclaration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdName' - Reserved for future use.
--
-- * 'bdType' - Reserved for future use.
blockerDeclaration ::
  -- | 'bdName'
  Text ->
  -- | 'bdType'
  BlockerType ->
  BlockerDeclaration
blockerDeclaration pName_ pType_ =
  BlockerDeclaration' {_bdName = pName_, _bdType = pType_}

-- | Reserved for future use.
bdName :: Lens' BlockerDeclaration Text
bdName = lens _bdName (\s a -> s {_bdName = a})

-- | Reserved for future use.
bdType :: Lens' BlockerDeclaration BlockerType
bdType = lens _bdType (\s a -> s {_bdType = a})

instance FromJSON BlockerDeclaration where
  parseJSON =
    withObject
      "BlockerDeclaration"
      (\x -> BlockerDeclaration' <$> (x .: "name") <*> (x .: "type"))

instance Hashable BlockerDeclaration

instance NFData BlockerDeclaration

instance ToJSON BlockerDeclaration where
  toJSON BlockerDeclaration' {..} =
    object
      (catMaybes [Just ("name" .= _bdName), Just ("type" .= _bdType)])
