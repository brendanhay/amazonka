{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.JSONInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.JSONInput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.JSONType

-- | Specifies JSON as object's input serialization format.
--
--
--
-- /See:/ 'jsonInput' smart constructor.
newtype JSONInput = JSONInput' {_jiType :: Maybe JSONType}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JSONInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jiType' - The type of JSON. Valid values: Document, Lines.
jsonInput ::
  JSONInput
jsonInput = JSONInput' {_jiType = Nothing}

-- | The type of JSON. Valid values: Document, Lines.
jiType :: Lens' JSONInput (Maybe JSONType)
jiType = lens _jiType (\s a -> s {_jiType = a})

instance Hashable JSONInput

instance NFData JSONInput

instance ToXML JSONInput where
  toXML JSONInput' {..} = mconcat ["Type" @= _jiType]
