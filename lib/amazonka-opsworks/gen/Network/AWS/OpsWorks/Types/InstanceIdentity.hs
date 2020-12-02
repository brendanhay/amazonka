{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.InstanceIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.InstanceIdentity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a description of an Amazon EC2 instance from the Amazon EC2 metadata service. For more information, see <https://docs.aws.amazon.com/sdkfornet/latest/apidocs/Index.html Instance Metadata and User Data> .
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
-- * 'iiSignature' - A signature that can be used to verify the document's accuracy and authenticity.
--
-- * 'iiDocument' - A JSON document that contains the metadata.
instanceIdentity ::
  InstanceIdentity
instanceIdentity =
  InstanceIdentity' {_iiSignature = Nothing, _iiDocument = Nothing}

-- | A signature that can be used to verify the document's accuracy and authenticity.
iiSignature :: Lens' InstanceIdentity (Maybe Text)
iiSignature = lens _iiSignature (\s a -> s {_iiSignature = a})

-- | A JSON document that contains the metadata.
iiDocument :: Lens' InstanceIdentity (Maybe Text)
iiDocument = lens _iiDocument (\s a -> s {_iiDocument = a})

instance Hashable InstanceIdentity

instance NFData InstanceIdentity

instance ToJSON InstanceIdentity where
  toJSON InstanceIdentity' {..} =
    object
      ( catMaybes
          [ ("Signature" .=) <$> _iiSignature,
            ("Document" .=) <$> _iiDocument
          ]
      )
