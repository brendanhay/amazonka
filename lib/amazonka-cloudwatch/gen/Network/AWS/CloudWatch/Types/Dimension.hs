{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Dimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Dimension where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A dimension is a name/value pair that is part of the identity of a metric. You can assign up to 10 dimensions to a metric. Because dimensions are part of the unique identifier for a metric, whenever you add a unique name/value pair to one of your metrics, you are creating a new variation of that metric.
--
--
--
-- /See:/ 'dimension' smart constructor.
data Dimension = Dimension' {_dName :: !Text, _dValue :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Dimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dName' - The name of the dimension. Dimension names cannot contain blank spaces or non-ASCII characters.
--
-- * 'dValue' - The value of the dimension. Dimension values cannot contain blank spaces or non-ASCII characters.
dimension ::
  -- | 'dName'
  Text ->
  -- | 'dValue'
  Text ->
  Dimension
dimension pName_ pValue_ =
  Dimension' {_dName = pName_, _dValue = pValue_}

-- | The name of the dimension. Dimension names cannot contain blank spaces or non-ASCII characters.
dName :: Lens' Dimension Text
dName = lens _dName (\s a -> s {_dName = a})

-- | The value of the dimension. Dimension values cannot contain blank spaces or non-ASCII characters.
dValue :: Lens' Dimension Text
dValue = lens _dValue (\s a -> s {_dValue = a})

instance FromXML Dimension where
  parseXML x = Dimension' <$> (x .@ "Name") <*> (x .@ "Value")

instance Hashable Dimension

instance NFData Dimension

instance ToQuery Dimension where
  toQuery Dimension' {..} =
    mconcat ["Name" =: _dName, "Value" =: _dValue]
