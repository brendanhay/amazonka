{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.FilterCondition
import Network.AWS.Route53AutoNaming.Types.ServiceFilterName

-- | A complex type that lets you specify the namespaces that you want to list services for.
--
--
--
-- /See:/ 'serviceFilter' smart constructor.
data ServiceFilter = ServiceFilter'
  { _sfCondition ::
      !(Maybe FilterCondition),
    _sfName :: !ServiceFilterName,
    _sfValues :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfCondition' - The operator that you want to use to determine whether a service is returned by @ListServices@ . Valid values for @Condition@ include the following:     * @EQ@ : When you specify @EQ@ , specify one namespace ID for @Values@ . @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ , specify a list of the IDs for the namespaces that you want @ListServices@ to return a list of services for.     * @BETWEEN@ : Not applicable.
--
-- * 'sfName' - Specify @NAMESPACE_ID@ .
--
-- * 'sfValues' - The values that are applicable to the value that you specify for @Condition@ to filter the list of services.
serviceFilter ::
  -- | 'sfName'
  ServiceFilterName ->
  ServiceFilter
serviceFilter pName_ =
  ServiceFilter'
    { _sfCondition = Nothing,
      _sfName = pName_,
      _sfValues = mempty
    }

-- | The operator that you want to use to determine whether a service is returned by @ListServices@ . Valid values for @Condition@ include the following:     * @EQ@ : When you specify @EQ@ , specify one namespace ID for @Values@ . @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ , specify a list of the IDs for the namespaces that you want @ListServices@ to return a list of services for.     * @BETWEEN@ : Not applicable.
sfCondition :: Lens' ServiceFilter (Maybe FilterCondition)
sfCondition = lens _sfCondition (\s a -> s {_sfCondition = a})

-- | Specify @NAMESPACE_ID@ .
sfName :: Lens' ServiceFilter ServiceFilterName
sfName = lens _sfName (\s a -> s {_sfName = a})

-- | The values that are applicable to the value that you specify for @Condition@ to filter the list of services.
sfValues :: Lens' ServiceFilter [Text]
sfValues = lens _sfValues (\s a -> s {_sfValues = a}) . _Coerce

instance Hashable ServiceFilter

instance NFData ServiceFilter

instance ToJSON ServiceFilter where
  toJSON ServiceFilter' {..} =
    object
      ( catMaybes
          [ ("Condition" .=) <$> _sfCondition,
            Just ("Name" .= _sfName),
            Just ("Values" .= _sfValues)
          ]
      )
