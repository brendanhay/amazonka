{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SigV4Authorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SigV4Authorization where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | For more information, see <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 signing process> .
--
--
--
-- /See:/ 'sigV4Authorization' smart constructor.
data SigV4Authorization = SigV4Authorization'
  { _svaSigningRegion ::
      !Text,
    _svaServiceName :: !Text,
    _svaRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SigV4Authorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svaSigningRegion' - The signing region.
--
-- * 'svaServiceName' - The service name to use while signing with Sig V4.
--
-- * 'svaRoleARN' - The ARN of the signing role.
sigV4Authorization ::
  -- | 'svaSigningRegion'
  Text ->
  -- | 'svaServiceName'
  Text ->
  -- | 'svaRoleARN'
  Text ->
  SigV4Authorization
sigV4Authorization pSigningRegion_ pServiceName_ pRoleARN_ =
  SigV4Authorization'
    { _svaSigningRegion = pSigningRegion_,
      _svaServiceName = pServiceName_,
      _svaRoleARN = pRoleARN_
    }

-- | The signing region.
svaSigningRegion :: Lens' SigV4Authorization Text
svaSigningRegion = lens _svaSigningRegion (\s a -> s {_svaSigningRegion = a})

-- | The service name to use while signing with Sig V4.
svaServiceName :: Lens' SigV4Authorization Text
svaServiceName = lens _svaServiceName (\s a -> s {_svaServiceName = a})

-- | The ARN of the signing role.
svaRoleARN :: Lens' SigV4Authorization Text
svaRoleARN = lens _svaRoleARN (\s a -> s {_svaRoleARN = a})

instance FromJSON SigV4Authorization where
  parseJSON =
    withObject
      "SigV4Authorization"
      ( \x ->
          SigV4Authorization'
            <$> (x .: "signingRegion")
            <*> (x .: "serviceName")
            <*> (x .: "roleArn")
      )

instance Hashable SigV4Authorization

instance NFData SigV4Authorization

instance ToJSON SigV4Authorization where
  toJSON SigV4Authorization' {..} =
    object
      ( catMaybes
          [ Just ("signingRegion" .= _svaSigningRegion),
            Just ("serviceName" .= _svaServiceName),
            Just ("roleArn" .= _svaRoleARN)
          ]
      )
