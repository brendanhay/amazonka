{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorRootCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorRootCause where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.ErrorRootCauseService

-- | The root cause of a trace summary error.
--
--
--
-- /See:/ 'errorRootCause' smart constructor.
data ErrorRootCause = ErrorRootCause'
  { _ercClientImpacting ::
      !(Maybe Bool),
    _ercServices :: !(Maybe [ErrorRootCauseService])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ErrorRootCause' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ercClientImpacting' - A flag that denotes that the root cause impacts the trace client.
--
-- * 'ercServices' - A list of services corresponding to an error. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
errorRootCause ::
  ErrorRootCause
errorRootCause =
  ErrorRootCause'
    { _ercClientImpacting = Nothing,
      _ercServices = Nothing
    }

-- | A flag that denotes that the root cause impacts the trace client.
ercClientImpacting :: Lens' ErrorRootCause (Maybe Bool)
ercClientImpacting = lens _ercClientImpacting (\s a -> s {_ercClientImpacting = a})

-- | A list of services corresponding to an error. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
ercServices :: Lens' ErrorRootCause [ErrorRootCauseService]
ercServices = lens _ercServices (\s a -> s {_ercServices = a}) . _Default . _Coerce

instance FromJSON ErrorRootCause where
  parseJSON =
    withObject
      "ErrorRootCause"
      ( \x ->
          ErrorRootCause'
            <$> (x .:? "ClientImpacting") <*> (x .:? "Services" .!= mempty)
      )

instance Hashable ErrorRootCause

instance NFData ErrorRootCause
