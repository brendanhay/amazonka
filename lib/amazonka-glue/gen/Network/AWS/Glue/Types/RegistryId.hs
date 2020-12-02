{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.RegistryId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RegistryId where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
--
--
-- /See:/ 'registryId' smart constructor.
data RegistryId = RegistryId'
  { _riRegistryName :: !(Maybe Text),
    _riRegistryARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegistryId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riRegistryName' - Name of the registry. Used only for lookup. One of @RegistryArn@ or @RegistryName@ has to be provided.
--
-- * 'riRegistryARN' - Arn of the registry to be updated. One of @RegistryArn@ or @RegistryName@ has to be provided.
registryId ::
  RegistryId
registryId =
  RegistryId' {_riRegistryName = Nothing, _riRegistryARN = Nothing}

-- | Name of the registry. Used only for lookup. One of @RegistryArn@ or @RegistryName@ has to be provided.
riRegistryName :: Lens' RegistryId (Maybe Text)
riRegistryName = lens _riRegistryName (\s a -> s {_riRegistryName = a})

-- | Arn of the registry to be updated. One of @RegistryArn@ or @RegistryName@ has to be provided.
riRegistryARN :: Lens' RegistryId (Maybe Text)
riRegistryARN = lens _riRegistryARN (\s a -> s {_riRegistryARN = a})

instance Hashable RegistryId

instance NFData RegistryId

instance ToJSON RegistryId where
  toJSON RegistryId' {..} =
    object
      ( catMaybes
          [ ("RegistryName" .=) <$> _riRegistryName,
            ("RegistryArn" .=) <$> _riRegistryARN
          ]
      )
