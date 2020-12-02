{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.FilterCondition
import Network.AWS.Route53AutoNaming.Types.NamespaceFilterName

-- | A complex type that identifies the namespaces that you want to list. You can choose to list public or private namespaces.
--
--
--
-- /See:/ 'namespaceFilter' smart constructor.
data NamespaceFilter = NamespaceFilter'
  { _nfCondition ::
      !(Maybe FilterCondition),
    _nfName :: !NamespaceFilterName,
    _nfValues :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NamespaceFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nfCondition' - The operator that you want to use to determine whether @ListNamespaces@ returns a namespace. Valid values for @condition@ include:     * @EQ@ : When you specify @EQ@ for the condition, you can choose to list only public namespaces or private namespaces, but not both. @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ for the condition, you can choose to list public namespaces, private namespaces, or both.      * @BETWEEN@ : Not applicable
--
-- * 'nfName' - Specify @TYPE@ .
--
-- * 'nfValues' - If you specify @EQ@ for @Condition@ , specify either @DNS_PUBLIC@ or @DNS_PRIVATE@ . If you specify @IN@ for @Condition@ , you can specify @DNS_PUBLIC@ , @DNS_PRIVATE@ , or both.
namespaceFilter ::
  -- | 'nfName'
  NamespaceFilterName ->
  NamespaceFilter
namespaceFilter pName_ =
  NamespaceFilter'
    { _nfCondition = Nothing,
      _nfName = pName_,
      _nfValues = mempty
    }

-- | The operator that you want to use to determine whether @ListNamespaces@ returns a namespace. Valid values for @condition@ include:     * @EQ@ : When you specify @EQ@ for the condition, you can choose to list only public namespaces or private namespaces, but not both. @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ for the condition, you can choose to list public namespaces, private namespaces, or both.      * @BETWEEN@ : Not applicable
nfCondition :: Lens' NamespaceFilter (Maybe FilterCondition)
nfCondition = lens _nfCondition (\s a -> s {_nfCondition = a})

-- | Specify @TYPE@ .
nfName :: Lens' NamespaceFilter NamespaceFilterName
nfName = lens _nfName (\s a -> s {_nfName = a})

-- | If you specify @EQ@ for @Condition@ , specify either @DNS_PUBLIC@ or @DNS_PRIVATE@ . If you specify @IN@ for @Condition@ , you can specify @DNS_PUBLIC@ , @DNS_PRIVATE@ , or both.
nfValues :: Lens' NamespaceFilter [Text]
nfValues = lens _nfValues (\s a -> s {_nfValues = a}) . _Coerce

instance Hashable NamespaceFilter

instance NFData NamespaceFilter

instance ToJSON NamespaceFilter where
  toJSON NamespaceFilter' {..} =
    object
      ( catMaybes
          [ ("Condition" .=) <$> _nfCondition,
            Just ("Name" .= _nfName),
            Just ("Values" .= _nfValues)
          ]
      )
