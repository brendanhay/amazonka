---
name: Release Checklist
about: Tasks to complete prior to cutting a new release
title: Release *.*.*
labels: ''
assignees: ''

---

- [ ] Check for new regions
- [ ] Update service bindings that have `Region -> Maybe Text` functions (usually returning an Account ID or Hosted Zone ID)
- [ ] Update Botocore
- [ ] Configure new services
- [ ] Regenerate everything
- [ ] Check against latest GHC versions
- [ ] Add new IMDS paths to our client
