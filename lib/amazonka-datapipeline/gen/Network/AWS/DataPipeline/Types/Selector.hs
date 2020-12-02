{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Selector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.Selector where

import Network.AWS.DataPipeline.Types.Operator
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A comparision that is used to determine whether a query should return this object.
--
--
--
-- /See:/ 'selector' smart constructor.
data Selector = Selector'
  { _sOperator :: !(Maybe Operator),
    _sFieldName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Selector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sOperator' - Undocumented member.
--
-- * 'sFieldName' - The name of the field that the operator will be applied to. The field name is the "key" portion of the field definition in the pipeline definition syntax that is used by the AWS Data Pipeline API. If the field is not set on the object, the condition fails.
selector ::
  Selector
selector = Selector' {_sOperator = Nothing, _sFieldName = Nothing}

-- | Undocumented member.
sOperator :: Lens' Selector (Maybe Operator)
sOperator = lens _sOperator (\s a -> s {_sOperator = a})

-- | The name of the field that the operator will be applied to. The field name is the "key" portion of the field definition in the pipeline definition syntax that is used by the AWS Data Pipeline API. If the field is not set on the object, the condition fails.
sFieldName :: Lens' Selector (Maybe Text)
sFieldName = lens _sFieldName (\s a -> s {_sFieldName = a})

instance Hashable Selector

instance NFData Selector

instance ToJSON Selector where
  toJSON Selector' {..} =
    object
      ( catMaybes
          [("operator" .=) <$> _sOperator, ("fieldName" .=) <$> _sFieldName]
      )
