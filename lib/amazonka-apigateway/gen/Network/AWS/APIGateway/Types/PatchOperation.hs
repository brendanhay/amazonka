{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.PatchOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.PatchOperation where

import Network.AWS.APIGateway.Types.Op
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A single patch operation to apply to the specified resource. Please refer to http://tools.ietf.org/html/rfc6902#section-4 for an explanation of how each operation is used.
--
-- /See:/ 'patchOperation' smart constructor.
data PatchOperation = PatchOperation'
  { _poOp :: !(Maybe Op),
    _poPath :: !(Maybe Text),
    _poValue :: !(Maybe Text),
    _poFrom :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PatchOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poOp' - An update operation to be performed with this PATCH request. The valid value can be @add@ , @remove@ , @replace@ or @copy@ . Not all valid operations are supported for a given resource. Support of the operations depends on specific operational contexts. Attempts to apply an unsupported operation on a resource will return an error message.
--
-- * 'poPath' - The @op@ operation's target, as identified by a <https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08 JSON Pointer> value that references a location within the targeted resource. For example, if the target resource has an updateable property of @{"name":"value"}@ , the path for this property is @/name@ . If the @name@ property value is a JSON object (e.g., @{"name": {"child/name": "child-value"}}@ ), the path for the @child/name@ property will be @/name/child~1name@ . Any slash ("/") character appearing in path names must be escaped with "~1", as shown in the example above. Each @op@ operation can have only one @path@ associated with it.
--
-- * 'poValue' - The new target value of the update operation. It is applicable for the @add@ or @replace@ operation. When using AWS CLI to update a property of a JSON value, enclose the JSON object with a pair of single quotes in a Linux shell, e.g., '{"a": ...}'. In a Windows shell, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> .
--
-- * 'poFrom' - The @copy@ update operation's source as identified by a @JSON-Pointer@ value referencing the location within the targeted resource to copy the value from. For example, to promote a canary deployment, you copy the canary deployment ID to the affiliated deployment ID by calling a PATCH request on a 'Stage' resource with @"op":"copy"@ , @"from":"/canarySettings/deploymentId"@ and @"path":"/deploymentId"@ .
patchOperation ::
  PatchOperation
patchOperation =
  PatchOperation'
    { _poOp = Nothing,
      _poPath = Nothing,
      _poValue = Nothing,
      _poFrom = Nothing
    }

-- | An update operation to be performed with this PATCH request. The valid value can be @add@ , @remove@ , @replace@ or @copy@ . Not all valid operations are supported for a given resource. Support of the operations depends on specific operational contexts. Attempts to apply an unsupported operation on a resource will return an error message.
poOp :: Lens' PatchOperation (Maybe Op)
poOp = lens _poOp (\s a -> s {_poOp = a})

-- | The @op@ operation's target, as identified by a <https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08 JSON Pointer> value that references a location within the targeted resource. For example, if the target resource has an updateable property of @{"name":"value"}@ , the path for this property is @/name@ . If the @name@ property value is a JSON object (e.g., @{"name": {"child/name": "child-value"}}@ ), the path for the @child/name@ property will be @/name/child~1name@ . Any slash ("/") character appearing in path names must be escaped with "~1", as shown in the example above. Each @op@ operation can have only one @path@ associated with it.
poPath :: Lens' PatchOperation (Maybe Text)
poPath = lens _poPath (\s a -> s {_poPath = a})

-- | The new target value of the update operation. It is applicable for the @add@ or @replace@ operation. When using AWS CLI to update a property of a JSON value, enclose the JSON object with a pair of single quotes in a Linux shell, e.g., '{"a": ...}'. In a Windows shell, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> .
poValue :: Lens' PatchOperation (Maybe Text)
poValue = lens _poValue (\s a -> s {_poValue = a})

-- | The @copy@ update operation's source as identified by a @JSON-Pointer@ value referencing the location within the targeted resource to copy the value from. For example, to promote a canary deployment, you copy the canary deployment ID to the affiliated deployment ID by calling a PATCH request on a 'Stage' resource with @"op":"copy"@ , @"from":"/canarySettings/deploymentId"@ and @"path":"/deploymentId"@ .
poFrom :: Lens' PatchOperation (Maybe Text)
poFrom = lens _poFrom (\s a -> s {_poFrom = a})

instance Hashable PatchOperation

instance NFData PatchOperation

instance ToJSON PatchOperation where
  toJSON PatchOperation' {..} =
    object
      ( catMaybes
          [ ("op" .=) <$> _poOp,
            ("path" .=) <$> _poPath,
            ("value" .=) <$> _poValue,
            ("from" .=) <$> _poFrom
          ]
      )
