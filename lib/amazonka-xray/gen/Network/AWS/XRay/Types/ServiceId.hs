{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ServiceId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ServiceId where

import Network.AWS.Lens
import Network.AWS.Prelude

-- |
--
--
--
-- /See:/ 'serviceId' smart constructor.
data ServiceId = ServiceId'
  { _siAccountId :: !(Maybe Text),
    _siNames :: !(Maybe [Text]),
    _siName :: !(Maybe Text),
    _siType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siAccountId' -
--
-- * 'siNames' -
--
-- * 'siName' -
--
-- * 'siType' -
serviceId ::
  ServiceId
serviceId =
  ServiceId'
    { _siAccountId = Nothing,
      _siNames = Nothing,
      _siName = Nothing,
      _siType = Nothing
    }

-- |
siAccountId :: Lens' ServiceId (Maybe Text)
siAccountId = lens _siAccountId (\s a -> s {_siAccountId = a})

-- |
siNames :: Lens' ServiceId [Text]
siNames = lens _siNames (\s a -> s {_siNames = a}) . _Default . _Coerce

-- |
siName :: Lens' ServiceId (Maybe Text)
siName = lens _siName (\s a -> s {_siName = a})

-- |
siType :: Lens' ServiceId (Maybe Text)
siType = lens _siType (\s a -> s {_siType = a})

instance FromJSON ServiceId where
  parseJSON =
    withObject
      "ServiceId"
      ( \x ->
          ServiceId'
            <$> (x .:? "AccountId")
            <*> (x .:? "Names" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "Type")
      )

instance Hashable ServiceId

instance NFData ServiceId
