{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfile where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Query argument-profile mapping for field-level encryption.
--
--
--
-- /See:/ 'queryArgProfile' smart constructor.
data QueryArgProfile = QueryArgProfile'
  { _qapQueryArg :: !Text,
    _qapProfileId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryArgProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qapQueryArg' - Query argument for field-level encryption query argument-profile mapping.
--
-- * 'qapProfileId' - ID of profile to use for field-level encryption query argument-profile mapping
queryArgProfile ::
  -- | 'qapQueryArg'
  Text ->
  -- | 'qapProfileId'
  Text ->
  QueryArgProfile
queryArgProfile pQueryArg_ pProfileId_ =
  QueryArgProfile'
    { _qapQueryArg = pQueryArg_,
      _qapProfileId = pProfileId_
    }

-- | Query argument for field-level encryption query argument-profile mapping.
qapQueryArg :: Lens' QueryArgProfile Text
qapQueryArg = lens _qapQueryArg (\s a -> s {_qapQueryArg = a})

-- | ID of profile to use for field-level encryption query argument-profile mapping
qapProfileId :: Lens' QueryArgProfile Text
qapProfileId = lens _qapProfileId (\s a -> s {_qapProfileId = a})

instance FromXML QueryArgProfile where
  parseXML x =
    QueryArgProfile' <$> (x .@ "QueryArg") <*> (x .@ "ProfileId")

instance Hashable QueryArgProfile

instance NFData QueryArgProfile

instance ToXML QueryArgProfile where
  toXML QueryArgProfile' {..} =
    mconcat
      ["QueryArg" @= _qapQueryArg, "ProfileId" @= _qapProfileId]
