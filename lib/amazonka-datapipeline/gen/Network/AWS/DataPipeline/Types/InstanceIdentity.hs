{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.InstanceIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.InstanceIdentity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identity information for the EC2 instance that is hosting the task runner. You can get this value by calling a metadata URI from the EC2 instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata> in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.
--
--
--
--
--
-- /See:/ 'instanceIdentity' smart constructor.
data InstanceIdentity = InstanceIdentity'
  { _iiSignature ::
      !(Maybe Text),
    _iiDocument :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiSignature' - A signature which can be used to verify the accuracy and authenticity of the information provided in the instance identity document.
--
-- * 'iiDocument' - A description of an EC2 instance that is generated when the instance is launched and exposed to the instance via the instance metadata service in the form of a JSON representation of an object.
instanceIdentity ::
  InstanceIdentity
instanceIdentity =
  InstanceIdentity' {_iiSignature = Nothing, _iiDocument = Nothing}

-- | A signature which can be used to verify the accuracy and authenticity of the information provided in the instance identity document.
iiSignature :: Lens' InstanceIdentity (Maybe Text)
iiSignature = lens _iiSignature (\s a -> s {_iiSignature = a})

-- | A description of an EC2 instance that is generated when the instance is launched and exposed to the instance via the instance metadata service in the form of a JSON representation of an object.
iiDocument :: Lens' InstanceIdentity (Maybe Text)
iiDocument = lens _iiDocument (\s a -> s {_iiDocument = a})

instance Hashable InstanceIdentity

instance NFData InstanceIdentity

instance ToJSON InstanceIdentity where
  toJSON InstanceIdentity' {..} =
    object
      ( catMaybes
          [ ("signature" .=) <$> _iiSignature,
            ("document" .=) <$> _iiDocument
          ]
      )
