{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Filter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a filter that is used to return a more specific list of results when describing tags.
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter' {_fValues :: !(Maybe [Text]), _fName :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fValues' - One or more filter values. Filter values are case-sensitive.
--
-- * 'fName' - The name of the filter. The valid values are: @auto-scaling-group@ , @key@ , @value@ , and @propagate-at-launch@ .
filter' ::
  -- | 'fName'
  Text ->
  Filter
filter' pName_ = Filter' {_fValues = Nothing, _fName = pName_}

-- | One or more filter values. Filter values are case-sensitive.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s {_fValues = a}) . _Default . _Coerce

-- | The name of the filter. The valid values are: @auto-scaling-group@ , @key@ , @value@ , and @propagate-at-launch@ .
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s {_fName = a})

instance Hashable Filter

instance NFData Filter

instance ToQuery Filter where
  toQuery Filter' {..} =
    mconcat
      [ "Values" =: toQuery (toQueryList "member" <$> _fValues),
        "Name" =: _fName
      ]
