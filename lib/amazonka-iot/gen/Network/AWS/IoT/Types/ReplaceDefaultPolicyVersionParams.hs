{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams where

import Network.AWS.IoT.Types.PolicyTemplateName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Parameters to define a mitigation action that adds a blank policy to restrict permissions.
--
--
--
-- /See:/ 'replaceDefaultPolicyVersionParams' smart constructor.
newtype ReplaceDefaultPolicyVersionParams = ReplaceDefaultPolicyVersionParams'
  { _rdpvpTemplateName ::
      PolicyTemplateName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplaceDefaultPolicyVersionParams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdpvpTemplateName' - The name of the template to be applied. The only supported value is @BLANK_POLICY@ .
replaceDefaultPolicyVersionParams ::
  -- | 'rdpvpTemplateName'
  PolicyTemplateName ->
  ReplaceDefaultPolicyVersionParams
replaceDefaultPolicyVersionParams pTemplateName_ =
  ReplaceDefaultPolicyVersionParams'
    { _rdpvpTemplateName =
        pTemplateName_
    }

-- | The name of the template to be applied. The only supported value is @BLANK_POLICY@ .
rdpvpTemplateName :: Lens' ReplaceDefaultPolicyVersionParams PolicyTemplateName
rdpvpTemplateName = lens _rdpvpTemplateName (\s a -> s {_rdpvpTemplateName = a})

instance FromJSON ReplaceDefaultPolicyVersionParams where
  parseJSON =
    withObject
      "ReplaceDefaultPolicyVersionParams"
      ( \x ->
          ReplaceDefaultPolicyVersionParams' <$> (x .: "templateName")
      )

instance Hashable ReplaceDefaultPolicyVersionParams

instance NFData ReplaceDefaultPolicyVersionParams

instance ToJSON ReplaceDefaultPolicyVersionParams where
  toJSON ReplaceDefaultPolicyVersionParams' {..} =
    object (catMaybes [Just ("templateName" .= _rdpvpTemplateName)])
