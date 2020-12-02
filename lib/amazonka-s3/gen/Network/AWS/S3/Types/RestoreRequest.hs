{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RestoreRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RestoreRequest where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.GlacierJobParameters
import Network.AWS.S3.Types.OutputLocation
import Network.AWS.S3.Types.RestoreRequestType
import Network.AWS.S3.Types.SelectParameters
import Network.AWS.S3.Types.Tier

-- | Container for restore job parameters.
--
--
--
-- /See:/ 'restoreRequest' smart constructor.
data RestoreRequest = RestoreRequest'
  { _rrDays :: !(Maybe Int),
    _rrSelectParameters :: !(Maybe SelectParameters),
    _rrOutputLocation :: !(Maybe OutputLocation),
    _rrTier :: !(Maybe Tier),
    _rrGlacierJobParameters :: !(Maybe GlacierJobParameters),
    _rrType :: !(Maybe RestoreRequestType),
    _rrDescription :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrDays' - Lifetime of the active copy in days. Do not use with restores that specify @OutputLocation@ . The Days element is required for regular restores, and must not be provided for select requests.
--
-- * 'rrSelectParameters' - Describes the parameters for Select job types.
--
-- * 'rrOutputLocation' - Describes the location where the restore job's output is stored.
--
-- * 'rrTier' - Retrieval tier at which the restore will be processed.
--
-- * 'rrGlacierJobParameters' - S3 Glacier related parameters pertaining to this job. Do not use with restores that specify @OutputLocation@ .
--
-- * 'rrType' - Type of restore request.
--
-- * 'rrDescription' - The optional description for the job.
restoreRequest ::
  RestoreRequest
restoreRequest =
  RestoreRequest'
    { _rrDays = Nothing,
      _rrSelectParameters = Nothing,
      _rrOutputLocation = Nothing,
      _rrTier = Nothing,
      _rrGlacierJobParameters = Nothing,
      _rrType = Nothing,
      _rrDescription = Nothing
    }

-- | Lifetime of the active copy in days. Do not use with restores that specify @OutputLocation@ . The Days element is required for regular restores, and must not be provided for select requests.
rrDays :: Lens' RestoreRequest (Maybe Int)
rrDays = lens _rrDays (\s a -> s {_rrDays = a})

-- | Describes the parameters for Select job types.
rrSelectParameters :: Lens' RestoreRequest (Maybe SelectParameters)
rrSelectParameters = lens _rrSelectParameters (\s a -> s {_rrSelectParameters = a})

-- | Describes the location where the restore job's output is stored.
rrOutputLocation :: Lens' RestoreRequest (Maybe OutputLocation)
rrOutputLocation = lens _rrOutputLocation (\s a -> s {_rrOutputLocation = a})

-- | Retrieval tier at which the restore will be processed.
rrTier :: Lens' RestoreRequest (Maybe Tier)
rrTier = lens _rrTier (\s a -> s {_rrTier = a})

-- | S3 Glacier related parameters pertaining to this job. Do not use with restores that specify @OutputLocation@ .
rrGlacierJobParameters :: Lens' RestoreRequest (Maybe GlacierJobParameters)
rrGlacierJobParameters = lens _rrGlacierJobParameters (\s a -> s {_rrGlacierJobParameters = a})

-- | Type of restore request.
rrType :: Lens' RestoreRequest (Maybe RestoreRequestType)
rrType = lens _rrType (\s a -> s {_rrType = a})

-- | The optional description for the job.
rrDescription :: Lens' RestoreRequest (Maybe Text)
rrDescription = lens _rrDescription (\s a -> s {_rrDescription = a})

instance Hashable RestoreRequest

instance NFData RestoreRequest

instance ToXML RestoreRequest where
  toXML RestoreRequest' {..} =
    mconcat
      [ "Days" @= _rrDays,
        "SelectParameters" @= _rrSelectParameters,
        "OutputLocation" @= _rrOutputLocation,
        "Tier" @= _rrTier,
        "GlacierJobParameters" @= _rrGlacierJobParameters,
        "Type" @= _rrType,
        "Description" @= _rrDescription
      ]
