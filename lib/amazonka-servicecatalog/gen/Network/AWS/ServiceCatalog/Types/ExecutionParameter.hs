{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ExecutionParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ExecutionParameter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of an execution parameter value that is passed to a self-service action when executed on a provisioned product.
--
--
--
-- /See:/ 'executionParameter' smart constructor.
data ExecutionParameter = ExecutionParameter'
  { _epDefaultValues ::
      !(Maybe [Text]),
    _epName :: !(Maybe Text),
    _epType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epDefaultValues' - The default values for the execution parameter.
--
-- * 'epName' - The name of the execution parameter.
--
-- * 'epType' - The execution parameter type.
executionParameter ::
  ExecutionParameter
executionParameter =
  ExecutionParameter'
    { _epDefaultValues = Nothing,
      _epName = Nothing,
      _epType = Nothing
    }

-- | The default values for the execution parameter.
epDefaultValues :: Lens' ExecutionParameter [Text]
epDefaultValues = lens _epDefaultValues (\s a -> s {_epDefaultValues = a}) . _Default . _Coerce

-- | The name of the execution parameter.
epName :: Lens' ExecutionParameter (Maybe Text)
epName = lens _epName (\s a -> s {_epName = a})

-- | The execution parameter type.
epType :: Lens' ExecutionParameter (Maybe Text)
epType = lens _epType (\s a -> s {_epType = a})

instance FromJSON ExecutionParameter where
  parseJSON =
    withObject
      "ExecutionParameter"
      ( \x ->
          ExecutionParameter'
            <$> (x .:? "DefaultValues" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "Type")
      )

instance Hashable ExecutionParameter

instance NFData ExecutionParameter
