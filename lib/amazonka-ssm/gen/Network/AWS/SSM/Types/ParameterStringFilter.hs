{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterStringFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterStringFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | One or more filters. Use a filter to return a more specific list of results.
--
--
--
-- /See:/ 'parameterStringFilter' smart constructor.
data ParameterStringFilter = ParameterStringFilter'
  { _psfValues ::
      !(Maybe (List1 Text)),
    _psfOption :: !(Maybe Text),
    _psfKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterStringFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psfValues' - The value you want to search for.
--
-- * 'psfOption' - For all filters used with 'DescribeParameters' , valid options include @Equals@ and @BeginsWith@ . The @Name@ filter additionally supports the @Contains@ option. (Exception: For filters using the key @Path@ , valid options include @Recursive@ and @OneLevel@ .) For filters used with 'GetParametersByPath' , valid options include @Equals@ and @BeginsWith@ . (Exception: For filters using @Label@ as the Key name, the only valid option is @Equals@ .)
--
-- * 'psfKey' - The name of the filter.
parameterStringFilter ::
  -- | 'psfKey'
  Text ->
  ParameterStringFilter
parameterStringFilter pKey_ =
  ParameterStringFilter'
    { _psfValues = Nothing,
      _psfOption = Nothing,
      _psfKey = pKey_
    }

-- | The value you want to search for.
psfValues :: Lens' ParameterStringFilter (Maybe (NonEmpty Text))
psfValues = lens _psfValues (\s a -> s {_psfValues = a}) . mapping _List1

-- | For all filters used with 'DescribeParameters' , valid options include @Equals@ and @BeginsWith@ . The @Name@ filter additionally supports the @Contains@ option. (Exception: For filters using the key @Path@ , valid options include @Recursive@ and @OneLevel@ .) For filters used with 'GetParametersByPath' , valid options include @Equals@ and @BeginsWith@ . (Exception: For filters using @Label@ as the Key name, the only valid option is @Equals@ .)
psfOption :: Lens' ParameterStringFilter (Maybe Text)
psfOption = lens _psfOption (\s a -> s {_psfOption = a})

-- | The name of the filter.
psfKey :: Lens' ParameterStringFilter Text
psfKey = lens _psfKey (\s a -> s {_psfKey = a})

instance Hashable ParameterStringFilter

instance NFData ParameterStringFilter

instance ToJSON ParameterStringFilter where
  toJSON ParameterStringFilter' {..} =
    object
      ( catMaybes
          [ ("Values" .=) <$> _psfValues,
            ("Option" .=) <$> _psfOption,
            Just ("Key" .= _psfKey)
          ]
      )
