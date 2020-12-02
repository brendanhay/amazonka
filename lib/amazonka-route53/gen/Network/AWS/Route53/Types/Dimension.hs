{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Dimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.Dimension where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

-- | For the metric that the CloudWatch alarm is associated with, a complex type that contains information about one dimension.
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
-- * 'dName' - For the metric that the CloudWatch alarm is associated with, the name of one dimension.
--
-- * 'dValue' - For the metric that the CloudWatch alarm is associated with, the value of one dimension.
dimension ::
  -- | 'dName'
  Text ->
  -- | 'dValue'
  Text ->
  Dimension
dimension pName_ pValue_ =
  Dimension' {_dName = pName_, _dValue = pValue_}

-- | For the metric that the CloudWatch alarm is associated with, the name of one dimension.
dName :: Lens' Dimension Text
dName = lens _dName (\s a -> s {_dName = a})

-- | For the metric that the CloudWatch alarm is associated with, the value of one dimension.
dValue :: Lens' Dimension Text
dValue = lens _dValue (\s a -> s {_dValue = a})

instance FromXML Dimension where
  parseXML x = Dimension' <$> (x .@ "Name") <*> (x .@ "Value")

instance Hashable Dimension

instance NFData Dimension
