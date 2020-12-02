{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceFilter where

import Network.AWS.CloudFormation.Types.StackInstanceFilterName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status that stack instances are filtered by.
--
--
--
-- /See:/ 'stackInstanceFilter' smart constructor.
data StackInstanceFilter = StackInstanceFilter'
  { _sifValues ::
      !(Maybe Text),
    _sifName :: !(Maybe StackInstanceFilterName)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackInstanceFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sifValues' - The status to filter by.
--
-- * 'sifName' - The type of filter to apply.
stackInstanceFilter ::
  StackInstanceFilter
stackInstanceFilter =
  StackInstanceFilter' {_sifValues = Nothing, _sifName = Nothing}

-- | The status to filter by.
sifValues :: Lens' StackInstanceFilter (Maybe Text)
sifValues = lens _sifValues (\s a -> s {_sifValues = a})

-- | The type of filter to apply.
sifName :: Lens' StackInstanceFilter (Maybe StackInstanceFilterName)
sifName = lens _sifName (\s a -> s {_sifName = a})

instance Hashable StackInstanceFilter

instance NFData StackInstanceFilter

instance ToQuery StackInstanceFilter where
  toQuery StackInstanceFilter' {..} =
    mconcat ["Values" =: _sifValues, "Name" =: _sifName]
