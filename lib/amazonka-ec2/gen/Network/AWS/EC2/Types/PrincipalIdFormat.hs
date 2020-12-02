{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrincipalIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrincipalIdFormat where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.IdFormat
import Network.AWS.Lens
import Network.AWS.Prelude

-- | PrincipalIdFormat description
--
--
--
-- /See:/ 'principalIdFormat' smart constructor.
data PrincipalIdFormat = PrincipalIdFormat'
  { _pifARN ::
      !(Maybe Text),
    _pifStatuses :: !(Maybe [IdFormat])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PrincipalIdFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pifARN' - PrincipalIdFormatARN description
--
-- * 'pifStatuses' - PrincipalIdFormatStatuses description
principalIdFormat ::
  PrincipalIdFormat
principalIdFormat =
  PrincipalIdFormat' {_pifARN = Nothing, _pifStatuses = Nothing}

-- | PrincipalIdFormatARN description
pifARN :: Lens' PrincipalIdFormat (Maybe Text)
pifARN = lens _pifARN (\s a -> s {_pifARN = a})

-- | PrincipalIdFormatStatuses description
pifStatuses :: Lens' PrincipalIdFormat [IdFormat]
pifStatuses = lens _pifStatuses (\s a -> s {_pifStatuses = a}) . _Default . _Coerce

instance FromXML PrincipalIdFormat where
  parseXML x =
    PrincipalIdFormat'
      <$> (x .@? "arn")
      <*> (x .@? "statusSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable PrincipalIdFormat

instance NFData PrincipalIdFormat
