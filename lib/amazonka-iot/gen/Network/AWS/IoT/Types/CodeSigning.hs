{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CodeSigning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CodeSigning where

import Network.AWS.IoT.Types.CustomCodeSigning
import Network.AWS.IoT.Types.StartSigningJobParameter
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the method to use when code signing a file.
--
--
--
-- /See:/ 'codeSigning' smart constructor.
data CodeSigning = CodeSigning'
  { _csCustomCodeSigning ::
      !(Maybe CustomCodeSigning),
    _csStartSigningJobParameter :: !(Maybe StartSigningJobParameter),
    _csAwsSignerJobId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeSigning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCustomCodeSigning' - A custom method for code signing a file.
--
-- * 'csStartSigningJobParameter' - Describes the code-signing job.
--
-- * 'csAwsSignerJobId' - The ID of the AWSSignerJob which was created to sign the file.
codeSigning ::
  CodeSigning
codeSigning =
  CodeSigning'
    { _csCustomCodeSigning = Nothing,
      _csStartSigningJobParameter = Nothing,
      _csAwsSignerJobId = Nothing
    }

-- | A custom method for code signing a file.
csCustomCodeSigning :: Lens' CodeSigning (Maybe CustomCodeSigning)
csCustomCodeSigning = lens _csCustomCodeSigning (\s a -> s {_csCustomCodeSigning = a})

-- | Describes the code-signing job.
csStartSigningJobParameter :: Lens' CodeSigning (Maybe StartSigningJobParameter)
csStartSigningJobParameter = lens _csStartSigningJobParameter (\s a -> s {_csStartSigningJobParameter = a})

-- | The ID of the AWSSignerJob which was created to sign the file.
csAwsSignerJobId :: Lens' CodeSigning (Maybe Text)
csAwsSignerJobId = lens _csAwsSignerJobId (\s a -> s {_csAwsSignerJobId = a})

instance FromJSON CodeSigning where
  parseJSON =
    withObject
      "CodeSigning"
      ( \x ->
          CodeSigning'
            <$> (x .:? "customCodeSigning")
            <*> (x .:? "startSigningJobParameter")
            <*> (x .:? "awsSignerJobId")
      )

instance Hashable CodeSigning

instance NFData CodeSigning

instance ToJSON CodeSigning where
  toJSON CodeSigning' {..} =
    object
      ( catMaybes
          [ ("customCodeSigning" .=) <$> _csCustomCodeSigning,
            ("startSigningJobParameter" .=) <$> _csStartSigningJobParameter,
            ("awsSignerJobId" .=) <$> _csAwsSignerJobId
          ]
      )
