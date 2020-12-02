{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Operator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.Operator where

import Network.AWS.DataPipeline.Types.OperatorType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a logical operation for comparing the value of a field with a specified value.
--
--
--
-- /See:/ 'operator' smart constructor.
data Operator = Operator'
  { _oValues :: !(Maybe [Text]),
    _oType :: !(Maybe OperatorType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Operator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oValues' - The value that the actual field value will be compared with.
--
-- * 'oType' - The logical operation to be performed: equal (@EQ@ ), equal reference (@REF_EQ@ ), less than or equal (@LE@ ), greater than or equal (@GE@ ), or between (@BETWEEN@ ). Equal reference (@REF_EQ@ ) can be used only with reference fields. The other comparison types can be used only with String fields. The comparison types you can use apply only to certain object fields, as detailed below.  The comparison operators EQ and REF_EQ act on the following fields:      * name    * @sphere    * parent    * @componentParent    * @instanceParent    * @status    * @scheduledStartTime    * @scheduledEndTime    * @actualStartTime    * @actualEndTime The comparison operators @GE@ , @LE@ , and @BETWEEN@ act on the following fields:      * @scheduledStartTime    * @scheduledEndTime    * @actualStartTime    * @actualEndTime Note that fields beginning with the at sign (@) are read-only and set by the web service. When you name fields, you should choose names containing only alpha-numeric values, as symbols may be reserved by AWS Data Pipeline. User-defined fields that you add to a pipeline should prefix their name with the string "my".
operator ::
  Operator
operator = Operator' {_oValues = Nothing, _oType = Nothing}

-- | The value that the actual field value will be compared with.
oValues :: Lens' Operator [Text]
oValues = lens _oValues (\s a -> s {_oValues = a}) . _Default . _Coerce

-- | The logical operation to be performed: equal (@EQ@ ), equal reference (@REF_EQ@ ), less than or equal (@LE@ ), greater than or equal (@GE@ ), or between (@BETWEEN@ ). Equal reference (@REF_EQ@ ) can be used only with reference fields. The other comparison types can be used only with String fields. The comparison types you can use apply only to certain object fields, as detailed below.  The comparison operators EQ and REF_EQ act on the following fields:      * name    * @sphere    * parent    * @componentParent    * @instanceParent    * @status    * @scheduledStartTime    * @scheduledEndTime    * @actualStartTime    * @actualEndTime The comparison operators @GE@ , @LE@ , and @BETWEEN@ act on the following fields:      * @scheduledStartTime    * @scheduledEndTime    * @actualStartTime    * @actualEndTime Note that fields beginning with the at sign (@) are read-only and set by the web service. When you name fields, you should choose names containing only alpha-numeric values, as symbols may be reserved by AWS Data Pipeline. User-defined fields that you add to a pipeline should prefix their name with the string "my".
oType :: Lens' Operator (Maybe OperatorType)
oType = lens _oType (\s a -> s {_oType = a})

instance Hashable Operator

instance NFData Operator

instance ToJSON Operator where
  toJSON Operator' {..} =
    object
      (catMaybes [("values" .=) <$> _oValues, ("type" .=) <$> _oType])
