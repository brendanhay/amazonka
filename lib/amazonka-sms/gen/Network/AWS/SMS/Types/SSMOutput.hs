{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.SSMOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.SSMOutput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.S3Location

-- | Contains the location of validation output.
--
--
--
-- /See:/ 'sSMOutput' smart constructor.
newtype SSMOutput = SSMOutput' {_ssmoS3Location :: Maybe S3Location}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SSMOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssmoS3Location' - Undocumented member.
sSMOutput ::
  SSMOutput
sSMOutput = SSMOutput' {_ssmoS3Location = Nothing}

-- | Undocumented member.
ssmoS3Location :: Lens' SSMOutput (Maybe S3Location)
ssmoS3Location = lens _ssmoS3Location (\s a -> s {_ssmoS3Location = a})

instance FromJSON SSMOutput where
  parseJSON =
    withObject
      "SSMOutput"
      (\x -> SSMOutput' <$> (x .:? "s3Location"))

instance Hashable SSMOutput

instance NFData SSMOutput
